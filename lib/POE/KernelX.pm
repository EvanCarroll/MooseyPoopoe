package POE::KernelX;
use MooseX::Singleton;
use MooseX::StrictConstructor;
use strict;

use POSIX qw(uname);
use Errno qw(ESRCH EINTR ECHILD EPERM EINVAL EEXIST EAGAIN EWOULDBLOCK);
use Carp qw(carp croak confess cluck);
use Sys::Hostname qw(hostname);
use IO::Handle ();
use File::Spec ();
use Scalar::Util ();

use POE::Helpers::Constants qw( :all );
use POE::Helpers::Error qw(:all);

with 'POE::Resource::Aliases';
with 'POE::Resource::Events';
with 'POE::Resource::Extrefs';
with 'POE::Resource::FileHandles';
with 'POE::Resource::Sessions';
with 'POE::Resource::SIDs';
with 'POE::Resource::Signals';
with 'POE::Resource::Statistics';

# People expect these to be lexical.
our ($poe_main_window);



has 'kr_queue' => (
	isa  => 'Object'
	, is => 'ro'
	, lazy => 1
	, default => sub {
		my $queue_class;
		eval {
			require POE::XS::Queue::Array;
			POE::XS::Queue::Array->import();
			$queue_class = "POE::XS::Queue::Array";
		};
		unless ($queue_class) {
			require POE::Queue::Array;
			POE::Queue::Array->import();
			$queue_class = "POE::Queue::Array";
		}
		$queue_class->new
	}
);

# Return the Kernel's "unique" ID.  There's only so much uniqueness
# available; machines on separate private 10/8 networks may have
# identical kernel IDs.  The chances of a collision are vanishingly
# small. The Kernel and Session IDs are based on Philip Gwyn's code.
has 'ID' => (
	isa  => 'Str'
	, is => 'ro'
	, clearer => '_clear_ID'
	, default => sub {
		my $hostname = eval { (uname)[1] };
		$hostname = hostname() unless defined $hostname;
		return ( "$hostname-" .  unpack('H*', pack('N*', time(), $$)) );
	}
);

has 'loop' => (
	isa => 'Str'
	, is => 'ro'
	, default => sub { $ENV{POE_EVENT_LOOP} || "POE::Loop::Select" }
	, initializer => sub {
		my ( $self, $value, $writerSub, $meta ) = @_;
		_test_loop( $value );
		$writerSub->($value);
	}
);

##sub BUILDARGS {
##  my @unknown = sort keys %$args;
##  croak "Unknown POE::Kernel import arguments: @unknown" if @unknown;
##}

#------------------------------------------------------------------------------
# Perform some optional setup.

BEGIN {
  local $SIG{'__DIE__'} = 'DEFAULT';

  # POE runs better with Time::HiRes, but it also runs without it.
  { no strict 'refs';

    # Allow users to turn off Time::HiRes usage for whatever reason.
    my $time_hires_default = 1;
    $time_hires_default = $ENV{USE_TIME_HIRES} if defined $ENV{USE_TIME_HIRES};
    if (defined &USE_TIME_HIRES) {
      $time_hires_default = USE_TIME_HIRES();
    }
    else {
      *USE_TIME_HIRES = sub () { $time_hires_default };
    }
  }
}

# Second BEGIN block so that USE_TIME_HIRES is treated as a constant.
BEGIN {
  eval {
    require Time::HiRes;
    Time::HiRes->import(qw(time sleep));
  } if USE_TIME_HIRES();

}

#==============================================================================
# Globals, or at least package-scoped things.  Data structures were
# moved into lexicals in 0.1201.

# A reference to the currently active session.  Used throughout the
# functions that act on the current session.
my $kr_active_session;
my $kr_active_event;
my $kr_run_warning = 0;

# Needs to be lexical so that POE::Resource::Events can see it
# change.  TODO - Something better?  Maybe we call a method in
# POE::Resource::Events to trigger the exception there?
use vars qw($kr_exception);

# This flag indicates that POE::Kernel's run() method was called.
# It's used to warn about forgetting $poe_kernel->run().
use constant {
	KR_RUN_CALLED    => 0x01 # $kernel->run() called
	, KR_RUN_SESSION => 0x02 # sessions created
	, KR_RUN_DONE    => 0x04 # run returned
};

# Temporary signal subtypes, used during signal dispatch semantics
# deprecation and reformation.
use constant ET_SIGNAL_RECURSIVE => 0x2000; # Explicitly requested signal.

# A hash of reserved names.  It's used to test whether someone is
# trying to use an internal event directly.

my %poes_own_events = (
  +EN_CHILD  => 1,
  +EN_GC     => 1,
  +EN_PARENT => 1,
  +EN_SCPOLL => 1,
  +EN_SIGNAL => 1,
  +EN_START  => 1,
  +EN_STOP   => 1,
  +EN_STAT   => 1,
);

# An "idle" POE::Kernel may still have events enqueued.  These events
# regulate polling for signals, profiling, and perhaps other aspecs of
# POE::Kernel's internal workings.
#
# XXX - There must be a better mechanism.
#
my $idle_queue_size = TRACE_STATISTICS ? 1 : 0;

sub _idle_queue_grow   { $idle_queue_size++; }
sub _idle_queue_shrink { $idle_queue_size--; }
sub _idle_queue_size   { $idle_queue_size;   }

#------------------------------------------------------------------------------
# Adapt POE::Kernel's personality to whichever event loop is present.

sub _find_loop {
  my ($mod) = @_;

  foreach my $dir (@INC) {
    return 1 if (-r "$dir/$mod");
  }
  return 0;
}

sub _load_loop {
  my $loop = shift;

  *poe_kernel_loop = sub { return "$loop" };

  # Modules can die with "not really dying" if they've loaded
  # something else.  This exception prevents the rest of the
  # originally used module from being parsed, so the module it's
  # handed off to takes over.
  with($loop);
  if ($@ and $@ !~ /not really dying/) {
    die(
      "*\n",
      "* POE can't use $loop:\n",
      "* $@\n",
      "*\n",
    );
  }
}

sub _test_loop {
  my $used_first = shift;
  local $SIG{__DIE__} = "DEFAULT";

  # First see if someone wants to load a POE::Loop or XS version
  # explicitly.
  if (defined $used_first) {
    _load_loop($used_first);
    return;
  }

  foreach my $file (keys %INC) {
    next if (substr ($file, -3) ne '.pm');
    my @split_dirs = File::Spec->splitdir($file);

    # Create a module name by replacing the path separators with
    # underscores and removing ".pm"
    my $module = join("_", @split_dirs);
    substr($module, -3) = "";

    # Skip the module name if it isn't legal.
    next if $module =~ /[^\w\.]/;

    # Try for the XS version first.  If it fails, try the plain
    # version.  If that fails, we're up a creek.
    $module = "POE/XS/Loop/$module.pm";
    unless (_find_loop($module)) {
      $module =~ s|XS/||;
      next unless (_find_loop($module));
    }

    if (defined $used_first and $used_first ne $module) {
      die(
        "*\n",
        "* POE can't use multiple event loops at once.\n",
        "* You used $used_first and $module.\n",
        "* Specify the loop you want as an argument to POE\n",
        "*  use POE qw(Loop::Select);\n",
        "* or;\n",
        "*  use POE::Kernel { loop => 'Select' };\n",
        "*\n",
      );
    }

    $used_first = $module;
  }

  # No loop found.  Default to our internal select() loop.
  unless (defined $used_first) {
    $used_first = "POE/XS/Loop/Select.pm";
    unless (_find_loop($used_first)) {
      $used_first =~ s/XS\///;
    }
  }

  substr($used_first, -3) = "";
  $used_first =~ s|/|::|g;
  _load_loop($used_first);
}

#------------------------------------------------------------------------------
# Include resource modules here.  Later, when we have the option of XS
# versions, we'll adapt this to include them if they're available.


###############################################################################
# Helpers.

### Resolve $whatever into a session reference, trying every method we
### can until something succeeds.

sub _resolve_session {
  my ($self, $whatever) = @_;
  my $session;

  # Resolve against sessions.
  $session = $self->_data_ses_resolve($whatever);
  return $session if defined $session;

  # Resolve against IDs.
  $session = $self->_data_sid_resolve($whatever);
  return $session if defined $session;

  # Resolve against aliases.
  $session = $self->_data_alias_resolve($whatever);
  return $session if defined $session;

  # Resolve against the Kernel itself.  Use "eq" instead of "==" here
  # because $whatever is often a string.
  return $whatever if $whatever eq $self;

  # We don't know what it is.
  return undef;
}

### Test whether POE has become idle.

sub _test_if_kernel_is_idle {
  my $self = shift;

  if (TRACE_REFCNT) {
    _warn(
      "<rc> ,----- Kernel Activity -----\n",
      "<rc> | Events : ", $self->kr_queue->get_item_count(), "\n",
      "<rc> | Files  : ", $self->_data_handle_count(), "\n",
      "<rc> | Extra  : ", $self->_data_extref_count(), "\n",
      "<rc> | Procs  : ", $self->_data_sig_child_procs(), "\n",
      "<rc> `---------------------------\n",
      "<rc> ..."
     );
  }

  unless (
    $self->kr_queue->get_item_count() > $idle_queue_size or
    $self->_data_handle_count() or
    $self->_data_extref_count() or
    $self->_data_sig_child_procs()
  ) {
    $self->_data_ev_enqueue(
      $self, $self, EN_SIGNAL, ET_SIGNAL, [ 'IDLE' ],
      __FILE__, __LINE__, undef, time(),
    ) if $self->_data_ses_count();
  }
}

### Explain why a session could not be resolved.

sub _explain_resolve_failure {
  my ($self, $whatever, $nonfatal) = @_;
  local $Carp::CarpLevel = 2;

  if (ASSERT_DATA and !$nonfatal) {
    _trap "<dt> Cannot resolve ``$whatever'' into a session reference";
  }

  $! = ESRCH;
  TRACE_RETVALS  and _carp "<rv> session not resolved: $!";
  ASSERT_RETVALS and _carp "<rv> session not resolved: $!";
}

### Explain why a function is returning unsuccessfully.

sub _explain_return {
  my ($self, $message) = @_;
  local $Carp::CarpLevel = 2;

  ASSERT_RETVALS and _confess "<rv> $message";
  TRACE_RETVALS  and _carp    "<rv> $message";
}

### Explain how the user made a mistake calling a function.

sub _explain_usage {
  my ($self, $message) = @_;
  local $Carp::CarpLevel = 2;

  ASSERT_USAGE   and _confess "<us> $message";
  ASSERT_RETVALS and _confess "<rv> $message";
  TRACE_RETVALS  and _carp    "<rv> $message";
}

#==============================================================================
# SIGNALS
#==============================================================================

#------------------------------------------------------------------------------
# Register or remove signals.

# Public interface for adding or removing signal handlers.

sub sig {
  my ($self, $signal, $event_name) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call sig() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined signal in sig()" unless defined $signal;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved assigning it to a signal"
    ) if defined($event_name) and exists($poes_own_events{$event_name});
  };

  if (defined $event_name) {
    $self->_data_sig_add($kr_active_session, $signal, $event_name);
  }
  else {
    $self->_data_sig_remove($kr_active_session, $signal);
  }
}

# Public interface for posting signal events.
# TODO - Like post(), signal() should return

sub signal {
  my ($self, $dest_session, $signal, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> undefined destination in signal()"
      unless defined $dest_session;
    _confess "<us> undefined signal in signal()" unless defined $signal;
  };

  my $session = $self->_resolve_session($dest_session);
  unless (defined $session) {
    $self->_explain_resolve_failure($dest_session);
    return;
  }

  $self->_data_ev_enqueue(
    $session, $kr_active_session,
    EN_SIGNAL, ET_SIGNAL, [ $signal, @etc ],
    (caller)[1,2], $kr_active_event, time(),
  );
  return 1;
}

# Public interface for flagging signals as handled.  This will replace
# the handlers' return values as an implicit flag.  Returns undef so
# it may be used as the last function in an event handler.

sub sig_handled {
  my $self = shift;
  $self->_data_sig_handled();

  if ($kr_active_event eq EN_SIGNAL) {
    _die(
      ",----- DEPRECATION ERROR -----\n",
      "| Session ", $self->_data_alias_loggable($kr_active_session), ":\n",
      "| handled a _signal event.  You must register a handler with sig().\n",
      "`-----------------------------\n",
    );
  }
}

# Attach a window or widget's destroy/closure to the UIDESTROY signal.

sub signal_ui_destroy {
  my ($self, $window) = @_;
  $self->loop_attach_uidestroy($window);
}

# Handle child PIDs being reaped.  Added 2006-09-15.

sub sig_child {
  my ($self, $pid, $event_name) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call sig_chld() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined process ID in sig_chld()" unless defined $pid;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved assigning it to a signal"
    ) if defined($event_name) and exists($poes_own_events{$event_name});
  };

  if (defined $event_name) {
    $self->_data_sig_pid_watch($kr_active_session, $pid, $event_name);
  }
  elsif ($self->_data_sig_pids_is_ses_watching($kr_active_session, $pid)) {
    $self->_data_sig_pid_ignore($kr_active_session, $pid);
  }
}

#==============================================================================
# KERNEL
#==============================================================================


##		foreach my $method ( $poe_kernel->meta->get_method_list ) {
##			# SKIP constants (XXX FUCKING BITCH $self->ID() IS NOT A CONSTANT)
##			next unless $method =~ /^[_a-z]|^ID/;
##			next if $method eq 'run' or $method eq 'meta' or $method eq 'singleton';
##			around ( $method => sub {
##				my ( $sub, $self, @args ) = @_;
##				my $reftype = Scalar::Util::reftype( $self );
##				if ( defined $reftype && $reftype eq 'ARRAY' ) {
##					$self = __PACKAGE__->instance;
##				}
##				#printf ( "\n%30s%10s\n\n\n\n\n", $method, Scalar::Util::reftype( $self ) );
##				$self->$sub( @args );
##			} );
##		}

sub BUILD {
	my $self = shift;
	$self->_data_sid_set($self->ID, $self);
    
		# Initialize subsystems.  The order is important.

    # We need events before sessions, and the kernel's session before
    # it can start polling for signals.  Statistics gathering requires
    # a polling event as well, so it goes late.
    $self->_initialize_kernel_session();
    $self->_data_stat_initialize() if TRACE_STATISTICS;
    $self->_data_sig_initialize();
    $self->_data_alias_initialize();

    # These other subsystems don't have strange interactions.
    $self->_data_handle_initialize($self->kr_queue);
};

#------------------------------------------------------------------------------
# Send an event to a session right now.  Used by _disp_select to
# expedite select() events, and used by run() to deliver posted events
# from the queue.

# Dispatch an event to its session.  A lot of work goes on here.

sub _dispatch_event {
  my (
    $self,
    $session, $source_session, $event, $type, $etc,
    $file, $line, $fromstate, $time, $seq
  ) = @_;

  if (ASSERT_EVENTS) {
    _confess "<ev> undefined dest session" unless defined $session;
    _confess "<ev> undefined source session" unless defined $source_session;
  };

  if (TRACE_EVENTS) {
    my $log_session = $session;
    $log_session =  $self->_data_alias_loggable($session) unless (
      $type & ET_START
    );
    my $string_etc = join(" ", map { defined() ? $_ : "(undef)" } @$etc);
    _warn(
      "<ev> Dispatching event $seq ``$event'' ($string_etc) from ",
      $self->_data_alias_loggable($source_session), " to $log_session"
    );
  }

  my $local_event = $event;

  $self->_stat_profile($event, $session) if TRACE_PROFILE;

  # Pre-dispatch processing.

  unless ($type & (ET_POST | ET_CALL)) {

    # A "select" event has just come out of the queue.  Reset its
    # actual state to its requested state before handling the event.

    if ($type & ET_SELECT) {
      $self->_data_handle_resume_requested_state(@$etc);
    }

    # Some sessions don't do anything in _start and expect their
    # creators to provide a start-up event.  This means we can't
    # &_collect_garbage at _start time.  Instead, we post a
    # garbage-collect event at start time, and &_collect_garbage at
    # delivery time.  This gives the session's creator time to do
    # things with it before we reap it.

    elsif ($type & ET_GC) {
      $self->_data_ses_collect_garbage($session);
      return 0;
    }

    # Preprocess signals.  This is where _signal is translated into
    # its registered handler's event name, if there is one.

    elsif ($type & ET_SIGNAL) {
      my $signal = $etc->[0];

      if (TRACE_SIGNALS) {
        _warn(
          "<sg> dispatching ET_SIGNAL ($signal) to ",
          $self->_data_alias_loggable($session)
        );
      }

      # Step 1a: Reset the handled-signal flags.

      local @POE::KernelX::kr_signaled_sessions;
      local $POE::KernelX::kr_signal_total_handled;
      local $POE::KernelX::kr_signal_type;

      $self->_data_sig_reset_handled($signal);

      # Step 1b: Collect a list of sessions to receive the signal.

      my @touched_sessions = ($session);
      my $touched_index = 0;
      while ($touched_index < @touched_sessions) {
        my $next_target = $touched_sessions[$touched_index];
        push @touched_sessions, $self->_data_ses_get_children($next_target);
        $touched_index++;
      }

      # Step 1c: The DIE signal propagates up through parents, too.

      if ($signal eq "DIE") {
        my $next_target = $self->_data_ses_get_parent($session);
        while (defined($next_target) and $next_target != $self) {
          unshift @touched_sessions, $next_target;
          $next_target = $self->_data_ses_get_parent($next_target);
        }
      }

      # Step 2: Propagate the signal to the explicit watchers in the
      # child tree.  Ensure the full tree is touched regardless
      # whether there are explicit watchers.

      if ($self->_data_sig_explicitly_watched($signal)) {
        my %signal_watchers = $self->_data_sig_watchers($signal);

        $touched_index = @touched_sessions;
        while ($touched_index--) {
          my $target_session = $touched_sessions[$touched_index];
          $self->_data_sig_touched_session($target_session);

          next unless exists $signal_watchers{$target_session};
          my $target_event = $signal_watchers{$target_session};

          if (TRACE_SIGNALS) {
            _warn(
              "<sg> propagating explicit signal $target_event ($signal) ",
              "to ", $self->_data_alias_loggable($target_session)
            );
          }

          # ET_SIGNAL_RECURSIVE is used here to avoid repropagating
          # the signal ad nauseam.
          $self->_dispatch_event(
            $target_session, $self,
            $target_event, ET_SIGNAL_RECURSIVE, [ @$etc ],
            $file, $line, $fromstate, time(), -__LINE__
          );
        }
      }
      else {
        $touched_index = @touched_sessions;
        while ($touched_index--) {
          $self->_data_sig_touched_session($touched_sessions[$touched_index]);
        }
      }

      # Step 3: Check to see if the signal was handled.

      $self->_data_sig_free_terminated_sessions();

      # If the signal was SIGDIE, then propagate the exception.

      my $handled_session_count = (_data_sig_handled_status())[0];
      if ($signal eq "DIE" and !$handled_session_count) {
        $kr_exception = $etc->[1]{error_str};
      }

      # Signal completely dispatched.  Thanks for flying!
      return;
    }
  }

  # The destination session doesn't exist.  This indicates sloppy
  # programming, possibly within POE::Kernel.

  unless ($self->_data_ses_exists($session)) {
    if (TRACE_EVENTS) {
      _warn(
        "<ev> discarding event $seq ``$event'' to nonexistent ",
        $self->_data_alias_loggable($session)
      );
    }
    return;
  }

  if (TRACE_EVENTS) {
    _warn(
    "<ev> dispatching event $seq ``$event'' to ",
      $self->_data_alias_loggable($session)
    );
    if ($event eq EN_SIGNAL) {
      _warn("<ev>     signal($etc->[0])");
    }
  }

  # Prepare to call the appropriate handler.  Push the current active
  # session on Perl's call stack.
  my $hold_active_session = $kr_active_session;
  $kr_active_session = $session;

  my $hold_active_event = $kr_active_event;
  $kr_active_event = $event;

  # Dispatch the event, at long last.
  my $before;
  if (TRACE_STATISTICS) {
    $before = time();
  }

  my $return;
  my $wantarray = wantarray;
  if (CATCH_EXCEPTIONS) {
    eval {
      if ($wantarray) {
        $return = [
          $session->_invoke_state(
            $source_session, $event, $etc, $file, $line, $fromstate
          )
        ];
      }
      elsif (defined $wantarray) {
        $return = $session->_invoke_state(
          $source_session, $event, $etc, $file, $line, $fromstate
        );
      }
      else {
        $session->_invoke_state(
          $source_session, $event, $etc, $file, $line, $fromstate
        );
      }
    };

    # local $@ doesn't work quite the way I expect, but there is a
    # bit of a problem if an eval{} occurs here because a signal is
    # dispatched or something.

    if (ref($@) or $@ ne '') {
      my $exception = $@;

      if(TRACE_EVENTS) {
        _warn(
          "<ev> exception occurred in $event when invoked on ",
          $self->_data_alias_loggable($session)
        );
      }

      # Exceptions in _stop are rethrown unconditionally.
      # We can't enqueue them--the session is about to go away.
      if ($type & ET_STOP) {
        $kr_exception = $exception;
      }
      else {
        $self->_data_ev_enqueue(
          $session, $self, EN_SIGNAL, ET_SIGNAL, [
            'DIE' => {
              source_session => $source_session,
              dest_session => $session,
              event => $event,
              file => $file,
              line => $line,
              from_state => $fromstate,
              error_str => $exception,
            },
          ], __FILE__, __LINE__, undef, time()
        );
      }
    }
  }
  else {
    if ($wantarray) {
      $return = [
        $session->_invoke_state(
          $source_session, $event, $etc, $file, $line, $fromstate
        )
      ];
    }
    elsif (defined $wantarray) {
      $return = $session->_invoke_state(
        $source_session, $event, $etc, $file, $line, $fromstate
      );
    }
    else {
      $session->_invoke_state(
        $source_session, $event, $etc, $file, $line, $fromstate
      );
    }
  }


  # Clear out the event arguments list, in case there are POE-ish
  # things in it. This allows them to destruct happily before we set
  # the current session back.
  #
  # We must preserve $_[ARG0] if the event is a signal.  It contains
  # the signal name, which is used by post-invoke processing to
  # determine future actions (such as whether to terminate the
  # session, or to promote SIGIDLE into SIGZOMBIE).
  #
  # TODO - @$etc contains @_[ARG0..$#_], which includes both watcher-
  # and user-supplied elements.  A more exciting solution might be to
  # have a table of events and their user-supplied indices, and wipe
  # them out programmatically.  splice(@$etc, $first_user{$type});
  # That would leave the watcher-supplied arguments alone.

  @$etc = ( );

  if (TRACE_STATISTICS) {
      my $after = time();
      my $elapsed = $after - $before;
      if ($type & ET_MASK_USER) {
        $self->_data_stat_add('user_seconds', $elapsed);
        $self->_data_stat_add('user_events', 1);
      }
  }

  # Stringify the handler's return value if it belongs in the POE
  # namespace.  $return's scope exists beyond the post-dispatch
  # processing, which includes POE's garbage collection.  The scope
  # bleed was known to break determinism in surprising ways.

  if (defined $return and substr(ref($return), 0, 5) eq 'POE::') {
    $return = "$return";
  }

  # Pop the active session, now that it's not active anymore.
  $kr_active_session = $hold_active_session;

  # Recover the event being processed.
  $kr_active_event = $hold_active_event;

  if (TRACE_EVENTS) {
    my $string_ret = $return;
    $string_ret = "undef" unless defined $string_ret;
    _warn("<ev> event $seq ``$event'' returns ($string_ret)\n");
  }

  # Bail out of post-dispatch processing if the session has been
  # stopped.  TODO This is extreme overhead.
  return unless $self->_data_ses_exists($session);

  # If this invocation is a user event, see if the destination session
  # needs to be garbage collected.  Also check the source session if
  # it's different from the destination.
  #
  # If the invocation is a call, and the destination session is
  # different from the calling one, test it for garbage collection.
  # We avoid testing if the source and destination are the same
  # because at some point we'll hit a user event that will catch it.
  #
  # TODO We test whether the sessions exist.  They should, but we've
  # been getting double-free errors lately.  I think we should avoid
  # the double free some other way, but this is the most expedient
  # method.
  #
  # TODO It turns out that POE::NFA->stop() may have discarded
  # sessions already, so we need to do the GC test anyway.  Maybe some
  # sort of mark-and-sweep can avoid redundant tests.

  if ($type & ET_POST) {
    $self->_data_ses_collect_garbage($session)
      if $self->_data_ses_exists($session);
    $self->_data_ses_collect_garbage($source_session)
      if ( $session != $source_session and
           $self->_data_ses_exists($source_session)
         );
  }
  elsif ($type & ET_CALL and $source_session != $session) {
    $self->_data_ses_collect_garbage($session)
      if $self->_data_ses_exists($session);
  }

  # These types of events require garbage collection afterwards, but
  # they don't need any other processing.

  elsif ($type & (ET_ALARM | ET_SELECT)) {
    $self->_data_ses_collect_garbage($session);
  }

  # Return what the handler did.  This is used for call().
  return @$return if wantarray;
  return $return;
}

#------------------------------------------------------------------------------
# POE's main loop!  Now with Tk and Event support!

# Do pre-run startup.  Initialize the event loop, and allocate a
# session structure to represent the Kernel.

sub _initialize_kernel_session {
  my $self = shift;

  $self->loop_initialize();

  $kr_exception = undef;
  $kr_active_session = $self;
  $self->_data_ses_allocate($self, $self->ID, undef);
}

# Do post-run cleanup.

sub _finalize_kernel {
  my $self = shift;

  # Disable signal watching since there's now no place for them to go.
  foreach ($self->_data_sig_get_safe_signals()) {
    $self->loop_ignore_signal($_);
  }

  # Remove the kernel session's signal watcher.
  $self->_data_sig_remove($self, "IDLE");

  # The main loop is done, no matter which event library ran it.
  $self->loop_finalize();
  $self->_data_extref_finalize();
  $self->_data_sid_finalize();
  $self->_data_sig_finalize();
  $self->_data_alias_finalize();
  $self->_data_handle_finalize();
  $self->_data_ev_finalize();
  $self->_data_ses_finalize();
  $self->_data_stat_finalize() if TRACE_PROFILE or TRACE_STATISTICS;
}

sub run_while {
  my ($self, $scalar_ref) = @_;
  1 while $$scalar_ref and $self->run_one_timeslice();
}

sub run_one_timeslice {
  my $self = shift;
  unless ($self->_data_ses_count()) {
    $self->_finalize_kernel();
    $kr_run_warning |= KR_RUN_DONE;
    return;
  }
  $self->loop_do_timeslice();
  return 1;
}

sub run {
	my $self = shift;

  # Flag that run() was called.
  $kr_run_warning |= KR_RUN_CALLED;

  # Don't run the loop if we have no sessions
  # Loop::Event will blow up, so we're doing this sanity check
  if ( $self->_data_ses_count() == 0 ) {
    # Emit noise only if we are under debug mode
    if ( ASSERT_DATA ) {
      _warn("Not running the event loop because we have no sessions!\n");
    }
  } else {
    # All signals must be explicitly watched now.  We do it here because
    # it's too early in initialize_kernel_session.
    $self->_data_sig_add($self, "IDLE", EN_SIGNAL);

    # Run the loop!
    $self->loop_run();

    # Cleanup
    $self->_finalize_kernel();
  }

  # Clean up afterwards.
  $kr_run_warning |= KR_RUN_DONE;
}

# Stops the kernel cold.  XXX Experimental!
# No events happen as a result of this, all structures are cleaned up
# except the kernel's.  Even the current session is cleaned up, which
# may introduce inconsistencies in the current session... as
# _dispatch_event() attempts to clean up for a defunct session.

sub stop {
  # So stop() can be called as a class method.
  my $self = shift;

  my @children = ($self);
  foreach my $session (@children) {
    push @children, $self->_data_ses_get_children($session);
  }

  # Remove the kernel itself.
  shift @children;

  # Walk backwards to avoid inconsistency errors.
  foreach my $session (reverse @children) {
    $self->_data_ses_free($session);
  }

  # So new sessions will not be child of the current defunct session.
  $kr_active_session = $self;

  $self->_clear_ID;

  return;
}

#------------------------------------------------------------------------------

sub DESTROY {
  my $self = shift;

  # Warn that a session never had the opportunity to run if one was
  # created but run() was never called.

  unless ($kr_run_warning & KR_RUN_CALLED) {
    if ($kr_run_warning & KR_RUN_SESSION) {
      _warn(
        "Sessions were started, but POE::Kernel's run() method was never\n",
        "called to execute them.  This usually happens because an error\n",
        "occurred before POE::Kernel->run() could be called.  Please fix\n",
        "any errors above this notice, and be sure that POE::Kernel->run()\n",
        "is called.\n",
      );
    }
  }
}

#------------------------------------------------------------------------------
# _invoke_state is what _dispatch_event calls to dispatch a transition
# event.  This is the kernel's _invoke_state so it can receive events.
# These are mostly signals, which are propagated down in
# _dispatch_event.

sub _invoke_state {
  my ($self, $source_session, $event, $etc) = @_;

  # This is an event loop to poll for child processes without needing
  # to catch SIGCHLD.

  if ($event eq EN_SCPOLL) {
    $self->_data_sig_handle_poll_event();
  }

  # A signal was posted.  Because signals propagate depth-first, this
  # _invoke_state is called last in the dispatch.  If the signal was
  # SIGIDLE, then post a SIGZOMBIE if the main queue is still idle.

  elsif ($event eq EN_SIGNAL) {
    if ($etc->[0] eq 'IDLE') {
      unless (
        $self->kr_queue->get_item_count() > $idle_queue_size or
        $self->_data_handle_count()
      ) {
        $self->_data_ev_enqueue(
          $self, $self, EN_SIGNAL, ET_SIGNAL, [ 'ZOMBIE' ],
          __FILE__, __LINE__, undef, time(),
        );
      }
    }
  }

  elsif ($event eq EN_STAT) {
    $self->_data_stat_tick();
  }

  return 0;
}

#==============================================================================
# SESSIONS
#==============================================================================

# Dispatch _start to a session, allocating it in the kernel's data
# structures as a side effect.

sub session_alloc {
  my ($self, $session, @args) = @_;

  # If we already returned, then we must reinitialize.  This is so
  # $poe_kernel->run() will work correctly more than once.
  if ($kr_run_warning & KR_RUN_DONE) {
    $kr_run_warning &= ~KR_RUN_DONE;
    $self->_initialize_kernel_session();
    $self->_data_stat_initialize() if TRACE_STATISTICS;
    $self->_data_sig_initialize();
  }

  if (ASSERT_DATA) {
    if ($self->_data_ses_exists($session)) {
      _trap(
        "<ss> ", $self->_data_alias_loggable($session), " already exists\a"
      );
    }
  }

  # Register that a session was created.
  $kr_run_warning |= KR_RUN_SESSION;

  # Allocate the session's data structure.  This must be done before
  # we dispatch anything regarding the new session.
  my $new_sid = $self->_data_sid_allocate();
  $self->_data_ses_allocate($session, $new_sid, $kr_active_session);

  my $loggable = $self->_data_alias_loggable($session);

  # Tell the new session that it has been created.  Catch the _start
  # state's return value so we can pass it to the parent with the
  # _child create.
  my $return = $self->_dispatch_event(
    $session, $kr_active_session,
    EN_START, ET_START, \@args,
    __FILE__, __LINE__, undef, time(), -__LINE__
  );
  unless($self->_data_ses_exists($session)) {
    if(TRACE_SESSIONS) {
      _warn("<ss> ", $loggable, " disappeared during ", EN_START);
    }
    return $return;
  }

  # If the child has not detached itself---that is, if its parent is
  # the currently active session---then notify the parent with a
  # _child create event.  Otherwise skip it, since we'd otherwise
  # throw a create without a lose.
  $self->_dispatch_event(
    $self->_data_ses_get_parent($session), $self,
    EN_CHILD, ET_CHILD, [ CHILD_CREATE, $session, $return ],
    __FILE__, __LINE__, undef, time(), -__LINE__
  );

  unless($self->_data_ses_exists($session)) {
    if(TRACE_SESSIONS) {
      _warn("<ss> ", $loggable, " disappeared during ", EN_CHILD, " dispatch");
    }
    return $return;
  }

  # Enqueue a delayed garbage-collection event so the session has time
  # to do its thing before it goes.
  $self->_data_ev_enqueue(
    $session, $session, EN_GC, ET_GC, [],
    __FILE__, __LINE__, undef, time(),
  );
}

# Detach a session from its parent.  This breaks the parent/child
# relationship between the current session and its parent.  Basically,
# the current session is given to the Kernel session.  Unlike with
# _stop, the current session's children follow their parent.
#
# TODO - Calling detach_myself() from _start means the parent receives
# a "_child lose" event without ever seeing "_child create".

sub detach_myself {
  my $self = shift;

  if (ASSERT_USAGE) {
    _confess "<us> must call detach_myself() from a running session"
      if $kr_active_session == $self;
  }

  # Can't detach from the kernel.
  if ($self->_data_ses_get_parent($kr_active_session) == $self) {
    $! = EPERM;
    return;
  }

  my $old_parent = $self->_data_ses_get_parent($kr_active_session);

  # Tell the old parent session that the child is departing.
  $self->_dispatch_event(
    $old_parent, $self,
    EN_CHILD, ET_CHILD, [ CHILD_LOSE, $kr_active_session, undef ],
    (caller)[1,2], undef, time(), -__LINE__
  );

  # Tell the new parent (kernel) that it's gaining a child.
  # (Actually it doesn't care, so we don't do that here, but this is
  # where the code would go if it ever does in the future.)

  # Tell the current session that its parentage is changing.
  $self->_dispatch_event(
    $kr_active_session, $self,
    EN_PARENT, ET_PARENT, [ $old_parent, $self ],
    (caller)[1,2], undef, time(), -__LINE__
  );

  $self->_data_ses_move_child($kr_active_session, $self);

  # Test the old parent for garbage.
  $self->_data_ses_collect_garbage($old_parent);

  # Success!
  return 1;
}

# Detach a child from this, the parent.  The session being detached
# must be a child of the current session.

sub detach_child {
  my ($self, $child) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call detach_child() from a running session"
      if $kr_active_session == $self;
  }

  my $child_session = $self->_resolve_session($child);
  unless (defined $child_session) {
    $self->_explain_resolve_failure($child);
    return;
  }

  # Can't detach if it belongs to the kernel.  TODO We shouldn't need
  # to check for this.
  if ($kr_active_session == $self) {
    $! = EPERM;
    return;
  }

  # Can't detach if it's not a child of the current session.
  unless ($self->_data_ses_is_child($kr_active_session, $child_session)) {
    $! = EPERM;
    return;
  }

  # Tell the current session that the child is departing.
  $self->_dispatch_event(
    $kr_active_session, $self,
    EN_CHILD, ET_CHILD, [ CHILD_LOSE, $child_session, undef ],
    (caller)[1,2], undef, time(), -__LINE__
  );

  # Tell the new parent (kernel) that it's gaining a child.
  # (Actually it doesn't care, so we don't do that here, but this is
  # where the code would go if it ever does in the future.)

  # Tell the child session that its parentage is changing.
  $self->_dispatch_event(
    $child_session, $self,
    EN_PARENT, ET_PARENT, [ $kr_active_session, $self ],
    (caller)[1,2], undef, time(), -__LINE__
  );

  $self->_data_ses_move_child($child_session, $self);

  # Test the old parent for garbage.
  $self->_data_ses_collect_garbage($kr_active_session);

  # Success!
  return 1;
}

### Helpful accessors.

sub get_active_session {
  return $kr_active_session;
}

sub get_active_event {
  return $kr_active_event;
}

# FIXME - Should this exist?
sub get_event_count {
  return $_[0]->kr_queue->get_item_count();
}

# FIXME - Should this exist?
sub get_next_event_time {
  return $_[0]->kr_queue->get_next_priority();
}

#==============================================================================
# EVENTS
#==============================================================================

#------------------------------------------------------------------------------
# Post an event to the queue.

sub post {
  my ($self, $dest_session, $event_name, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> destination is undefined in post()"
      unless defined $dest_session;
    _confess "<us> event is undefined in post()" unless defined $event_name;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by posting it"
    ) if exists $poes_own_events{$event_name};
  };

  # Attempt to resolve the destination session reference against
  # various things.

  my $session = $self->_resolve_session($dest_session);
  unless (defined $session) {
    $self->_explain_resolve_failure($dest_session);
    return;
  }

  # Enqueue the event for "now", which simulates FIFO in our
  # time-ordered queue.

  $self->_data_ev_enqueue(
    $session, $kr_active_session, $event_name, ET_POST, \@etc,
    (caller)[1,2], $kr_active_event, time(),
  );
  return 1;
}

#------------------------------------------------------------------------------
# Post an event to the queue for the current session.

sub yield {
  my ($self, $event_name, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call yield() from a running session"
      if $kr_active_session == $self;
    _confess "<us> event name is undefined in yield()"
      unless defined $event_name;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by yielding it"
    ) if exists $poes_own_events{$event_name};
  };

  $self->_data_ev_enqueue(
    $kr_active_session, $kr_active_session, $event_name, ET_POST, \@etc,
    (caller)[1,2], $kr_active_event, time(),
  );

  undef;
}

#------------------------------------------------------------------------------
# Call an event handler directly.

sub call {
  my ($self, $dest_session, $event_name, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> destination is undefined in call()"
      unless defined $dest_session;
    _confess "<us> event is undefined in call()" unless defined $event_name;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by calling it"
    ) if exists $poes_own_events{$event_name};
  };

  # Attempt to resolve the destination session reference against
  # various things.

  my $session = $self->_resolve_session($dest_session);
  unless (defined $session) {
    $self->_explain_resolve_failure($dest_session);
    return;
  }

  # Dispatch the event right now, bypassing the queue altogether.
  # This tends to be a Bad Thing to Do.

  # TODO The difference between synchronous and asynchronous events
  # should be made more clear in the documentation, so that people
  # have a tendency not to abuse them.  I discovered in xws that that
  # mixing the two types makes it harder than necessary to write
  # deterministic programs, but the difficulty can be ameliorated if
  # programmers set some base rules and stick to them.

  $self->_stat_profile($event_name, $session) if TRACE_PROFILE;

  if (wantarray) {
    my @return_value = (
      ($session == $kr_active_session)
      ? $session->_invoke_state(
        $session, $event_name, \@etc, (caller)[1,2],
        $kr_active_event
      )
      : $self->_dispatch_event(
        $session, $kr_active_session,
        $event_name, ET_CALL, \@etc,
        (caller)[1,2], $kr_active_event, time(), -__LINE__
      )
    );

    $! = 0;
    return @return_value;
  }

  if (defined wantarray) {
    my $return_value = (
      $session == $kr_active_session
      ? $session->_invoke_state(
        $session, $event_name, \@etc, (caller)[1,2],
        $kr_active_event
      )
      : $self->_dispatch_event(
        $session, $kr_active_session,
        $event_name, ET_CALL, \@etc,
        (caller)[1,2], $kr_active_event, time(), -__LINE__
      )
    );

    $! = 0;
    return $return_value;
  }

  if ($session == $kr_active_session) {
    $session->_invoke_state(
      $session, $event_name, \@etc, (caller)[1,2],
      $kr_active_event
    );
  }
  else {
    $self->_dispatch_event(
      $session, $kr_active_session,
      $event_name, ET_CALL, \@etc,
      (caller)[1,2], $kr_active_event, time(), -__LINE__
    );
  }

  $! = 0;
  return;
}

#==============================================================================
# DELAYED EVENTS
#==============================================================================

sub alarm {
  my ($self, $event_name, $time, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call alarm() from a running session"
      if $kr_active_session == $self;
    _confess "<us> event name is undefined in alarm()"
      unless defined $event_name;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by setting an alarm for it"
    ) if exists $poes_own_events{$event_name};
  };

  unless (defined $event_name) {
    $self->_explain_return("invalid parameter to alarm() call");
    return EINVAL;
  }

  $self->_data_ev_clear_alarm_by_name($kr_active_session, $event_name);

  # Add the new alarm if it includes a time.  Calling _data_ev_enqueue
  # directly is faster than calling alarm_set to enqueue it.
  if (defined $time) {
    $self->_data_ev_enqueue
      ( $kr_active_session, $kr_active_session,
        $event_name, ET_ALARM, [ @etc ],
        (caller)[1,2], $kr_active_event, $time,
      );
  }
  else {
    # The event queue has become empty?  Stop the time watcher.
    $self->loop_pause_time_watcher() unless $self->kr_queue->get_item_count();
  }

  return 0;
}

# Add an alarm without clobbering previous alarms of the same name.
sub alarm_add {
  my ($self, $event_name, $time, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call alarm_add() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined event name in alarm_add()"
      unless defined $event_name;
    _confess "<us> undefined time in alarm_add()" unless defined $time;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by adding an alarm for it"
    ) if exists $poes_own_events{$event_name};
  };

  unless (defined $event_name and defined $time) {
    $self->_explain_return("invalid parameter to alarm_add() call");
    return EINVAL;
  }

  $self->_data_ev_enqueue
    ( $kr_active_session, $kr_active_session,
      $event_name, ET_ALARM, [ @etc ],
      (caller)[1,2], $kr_active_event, $time,
    );

  return 0;
}

# Add a delay, which is just an alarm relative to the current time.
sub delay {
  my ($self, $event_name, $delay, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call delay() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined event name in delay()" unless defined $event_name;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by setting a delay for it"
    ) if exists $poes_own_events{$event_name};
  };

  unless (defined $event_name) {
    $self->_explain_return("invalid parameter to delay() call");
    return EINVAL;
  }

  if (defined $delay) {
    $self->alarm($event_name, time() + $delay, @etc);
  }
  else {
    $self->alarm($event_name);
  }

  return 0;
}

# Add a delay without clobbering previous delays of the same name.
sub delay_add {
  my ($self, $event_name, $delay, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call delay_add() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined event name in delay_add()"
      unless defined $event_name;
    _confess "<us> undefined time in delay_add()" unless defined $delay;
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by adding a delay for it"
    ) if exists $poes_own_events{$event_name};
  };

  unless (defined $event_name and defined $delay) {
    $self->_explain_return("invalid parameter to delay_add() call");
    return EINVAL;
  }

  $self->alarm_add($event_name, time() + $delay, @etc);

  return 0;
}

#------------------------------------------------------------------------------
# New style alarms.

# Set an alarm.  This does more *and* less than plain alarm().  It
# only sets alarms (that's the less part), but it also returns an
# alarm ID (that's the more part).

sub alarm_set {
  my ($self, $event_name, $time, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call alarm_set() from a running session"
      if $kr_active_session == $self;
  }

  unless (defined $event_name) {
    $self->_explain_usage("undefined event name in alarm_set()");
    $! = EINVAL;
    return;
  }

  unless (defined $time) {
    $self->_explain_usage("undefined time in alarm_set()");
    $! = EINVAL;
    return;
  }

  if (ASSERT_USAGE) {
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by setting an alarm for it"
    ) if exists $poes_own_events{$event_name};
  }

  return $self->_data_ev_enqueue
    ( $kr_active_session, $kr_active_session, $event_name, ET_ALARM, [ @etc ],
      (caller)[1,2], $kr_active_event, $time,
    );
}

# Remove an alarm by its ID.  TODO Now that alarms and events have
# been recombined, this will remove an event by its ID.  However,
# nothing returns an event ID, so nobody knows what to remove.

sub alarm_remove {
  my ($self, $alarm_id) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call alarm_remove() from a running session"
      if $kr_active_session == $self;
  }

  unless (defined $alarm_id) {
    $self->_explain_usage("undefined alarm id in alarm_remove()");
    $! = EINVAL;
    return;
  }

  my ($time, $event) =
    $self->_data_ev_clear_alarm_by_id($kr_active_session, $alarm_id);
  return unless defined $time;

  # In a list context, return the alarm that was removed.  In a scalar
  # context, return a reference to the alarm that was removed.  In a
  # void context, return nothing.  Either way this returns a defined
  # value when someone needs something useful from it.

  return unless defined wantarray;
  return ( $event->[POE::Resource::Events->EV_NAME], $time, @{$event->[POE::Resource::Events->EV_ARGS]} ) if wantarray;
  return [ $event->[POE::Resource::Events->EV_NAME], $time, @{$event->[POE::Resource::Events->EV_ARGS]} ];
}

# Move an alarm to a new time.  This virtually removes the alarm and
# re-adds it somewhere else.  In reality, adjust_priority() is
# optimized for this sort of thing.

sub alarm_adjust {
  my ($self, $alarm_id, $delta) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call alarm_adjust() from a running session"
      if $kr_active_session == $self;
  }

  unless (defined $alarm_id) {
    $self->_explain_usage("undefined alarm id in alarm_adjust()");
    $! = EINVAL;
    return;
  }

  unless (defined $delta) {
    $self->_explain_usage("undefined alarm delta in alarm_adjust()");
    $! = EINVAL;
    return;
  }

  my $my_alarm = sub {
    $_[0]->[POE::Resource::Events->EV_SESSION] == $kr_active_session;
  };
  return $self->kr_queue->adjust_priority($alarm_id, $my_alarm, $delta);
}

# A convenient function for setting alarms relative to now.  It also
# uses whichever time() POE::Kernel can find, which may be
# Time::HiRes'.

sub delay_set {
  # Always always always grab time() ASAP, so that the eventual
  # time we set the alarm for is as close as possible to the time
  # at which they ASKED for the delay, not when we actually set it.
  my $t = time();

  # And now continue as normal
  my ($self, $event_name, $seconds, @etc) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call delay_set() from a running session"
      if $kr_active_session == $self;
  }

  unless (defined $event_name) {
    $self->_explain_usage("undefined event name in delay_set()");
    $! = EINVAL;
    return;
  }

  if (ASSERT_USAGE) {
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by setting a delay for it"
    ) if exists $poes_own_events{$event_name};
  }

  unless (defined $seconds) {
    $self->_explain_usage("undefined seconds in delay_set()");
    $! = EINVAL;
    return;
  }

  return $self->_data_ev_enqueue
    ( $kr_active_session, $kr_active_session, $event_name, ET_ALARM, [ @etc ],
      (caller)[1,2], $kr_active_event, $t + $seconds,
    );
}

# Move a delay to a new offset from time().  As with alarm_adjust(),
# this is optimized internally for this sort of activity.

sub delay_adjust {
  my ($self, $alarm_id, $seconds) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call delay_adjust() from a running session"
      if $kr_active_session == $self;
  }

  unless (defined $alarm_id) {
    $self->_explain_usage("undefined delay id in delay_adjust()");
    $! = EINVAL;
    return;
  }

  unless (defined $seconds) {
    $self->_explain_usage("undefined delay seconds in delay_adjust()");
    $! = EINVAL;
    return;
  }

  my $my_delay = sub {
    $_[0]->[POE::Resource::Events->EV_SESSION] == $kr_active_session;
  };

  if (TRACE_EVENTS) {
    _warn("<ev> adjusted event $alarm_id by $seconds seconds");
  }

  return $self->kr_queue->set_priority($alarm_id, $my_delay, time() + $seconds);
}

# Remove all alarms for the current session.

sub alarm_remove_all {
  my $self = shift;

  if (ASSERT_USAGE) {
    _confess "<us> must call alarm_remove_all() from a running session"
      if $kr_active_session == $self;
  }

  # This should never happen, actually.
  _trap "unknown session in alarm_remove_all call"
    unless $self->_data_ses_exists($kr_active_session);

  # Free every alarm owned by the session.  This code is ripped off
  # from the _stop code to flush everything.

  my @removed = $self->_data_ev_clear_alarm_by_session($kr_active_session);

  return unless defined wantarray;
  return @removed if wantarray;
  return \@removed;
}

#==============================================================================
# SELECTS
#==============================================================================

sub _internal_select {
  my ($self, $session, $handle, $event_name, $mode, $args) = @_;

  # If an event is included, then we're defining a filehandle watcher.

  if ($event_name) {
    $self->_data_handle_add($handle, $mode, $session, $event_name, $args);
  }
  else {
    $self->_data_handle_remove($handle, $mode, $session);
  }
}

# A higher-level select() that manipulates read, write and expedite
# selects together.

sub select {
  my ($self, $handle, $event_r, $event_w, $event_e, @args) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call select() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined filehandle in select()" unless defined $handle;
    _confess "<us> invalid filehandle in select()"
      unless defined fileno($handle);
    foreach ($event_r, $event_w, $event_e) {
      next unless defined $_;
      _carp(
        "<us> The '$_' event is one of POE's own.  Its " .
        "effect cannot be achieved by setting a file watcher to it"
      ) if exists($poes_own_events{$_});
    }
  }

  $self->_internal_select(
    $kr_active_session, $handle, $event_r, MODE_RD, \@args
  );
  $self->_internal_select(
    $kr_active_session, $handle, $event_w, MODE_WR, \@args
  );
  $self->_internal_select(
    $kr_active_session, $handle, $event_e, MODE_EX, \@args
  );
  return 0;
}

# Only manipulate the read select.
sub select_read {
  my ($self, $handle, $event_name, @args) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call select_read() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined filehandle in select_read()"
      unless defined $handle;
    _confess "<us> invalid filehandle in select_read()"
      unless defined fileno($handle);
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by setting a file watcher to it"
    ) if defined($event_name) and exists($poes_own_events{$event_name});
  };

  $self->_internal_select(
    $kr_active_session, $handle, $event_name, MODE_RD, \@args
  );
  return 0;
}

# Only manipulate the write select.
sub select_write {
  my ($self, $handle, $event_name, @args) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call select_write() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined filehandle in select_write()"
      unless defined $handle;
    _confess "<us> invalid filehandle in select_write()"
      unless defined fileno($handle);
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by setting a file watcher to it"
    ) if defined($event_name) and exists($poes_own_events{$event_name});
  };

  $self->_internal_select(
    $kr_active_session, $handle, $event_name, MODE_WR, \@args
  );
  return 0;
}

# Only manipulate the expedite select.
sub select_expedite {
  my ($self, $handle, $event_name, @args) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call select_expedite() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined filehandle in select_expedite()"
      unless defined $handle;
    _confess "<us> invalid filehandle in select_expedite()"
      unless defined fileno($handle);
    _carp(
      "<us> The '$event_name' event is one of POE's own.  Its " .
      "effect cannot be achieved by setting a file watcher to it"
    ) if defined($event_name) and exists($poes_own_events{$event_name});
  };

  $self->_internal_select(
    $kr_active_session, $handle, $event_name, MODE_EX, \@args
  );
  return 0;
}

# Turn off a handle's write mode bit without doing
# garbage-collection things.
sub select_pause_write {
  my ($self, $handle) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call select_pause_write() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined filehandle in select_pause_write()"
      unless defined $handle;
    _confess "<us> invalid filehandle in select_pause_write()"
      unless defined fileno($handle);
  };

  return 0 unless $self->_data_handle_is_good($handle, MODE_WR);

  $self->_data_handle_pause($handle, MODE_WR);

  return 1;
}

# Turn on a handle's write mode bit without doing garbage-collection
# things.
sub select_resume_write {
  my ($self, $handle) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call select_resume_write() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined filehandle in select_resume_write()"
      unless defined $handle;
    _confess "<us> invalid filehandle in select_resume_write()"
      unless defined fileno($handle);
  };

  return 0 unless $self->_data_handle_is_good($handle, MODE_WR);

  $self->_data_handle_resume($handle, MODE_WR);

  return 1;
}

# Turn off a handle's read mode bit without doing garbage-collection
# things.
sub select_pause_read {
  my ($self, $handle) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call select_pause_read() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined filehandle in select_pause_read()"
      unless defined $handle;
    _confess "<us> invalid filehandle in select_pause_read()"
      unless defined fileno($handle);
  };

  return 0 unless $self->_data_handle_is_good($handle, MODE_RD);

  $self->_data_handle_pause($handle, MODE_RD);

  return 1;
}

# Turn on a handle's read mode bit without doing garbage-collection
# things.
sub select_resume_read {
  my ($self, $handle) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> must call select_resume_read() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined filehandle in select_resume_read()"
      unless defined $handle;
    _confess "<us> invalid filehandle in select_resume_read()"
      unless defined fileno($handle);
  };

  return 0 unless $self->_data_handle_is_good($handle, MODE_RD);

  $self->_data_handle_resume($handle, MODE_RD);

  return 1;
}

#==============================================================================
# Aliases: These functions expose the internal alias accessors with
# extra fun parameter/return value checking.
#==============================================================================

### Set an alias in the current session.

sub alias_set {
  my ($self, $name) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> undefined alias in alias_set()" unless defined $name;
  };

  # Don't overwrite another session's alias.
  my $existing_session = $self->_data_alias_resolve($name);
  if (defined $existing_session) {
    if ($existing_session != $kr_active_session) {
      $self->_explain_usage("alias '$name' is in use by another session");
      return EEXIST;
    }
    return 0;
  }

  $self->_data_alias_add($kr_active_session, $name);
  return 0;
}

### Remove an alias from the current session.

sub alias_remove {
  my ($self, $name) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> undefined alias in alias_remove()" unless defined $name;
  };

  my $existing_session = $self->_data_alias_resolve($name);

  unless (defined $existing_session) {
    $self->_explain_usage("alias does not exist");
    return ESRCH;
  }

  if ($existing_session != $kr_active_session) {
    $self->_explain_usage("alias does not belong to current session");
    return EPERM;
  }

  $self->_data_alias_remove($kr_active_session, $name);
  return 0;
}

### Resolve an alias into a session.

sub alias_resolve {
  my ($self, $name) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> undefined alias in alias_resolve()" unless defined $name;
  };

  my $session = $self->_resolve_session($name);
  unless (defined $session) {
    $self->_explain_resolve_failure($name, "nonfatal");
    return;
  }

  $session;
}

### List the aliases for a given session.

sub alias_list {
  my ($self, $search_session) = @_;
  my $session =
    $self->_resolve_session($search_session || $kr_active_session);

  unless (defined $session) {
    $self->_explain_resolve_failure($search_session, "nonfatal");
    return;
  }

  # Return whatever can be found.
  my @alias_list = $self->_data_alias_list($session);
  return wantarray() ? @alias_list : $alias_list[0];
}

# Resolve an ID to a session reference.  This function is virtually
# moot now that _resolve_session does it too.  This explicit call will
# be faster, though, so it's kept for things that can benefit from it.

sub ID_id_to_session {
  my ($self, $id) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> undefined ID in ID_id_to_session()" unless defined $id;
  };

  my $session = $self->_data_sid_resolve($id);
  return $session if defined $session;

  $self->_explain_return("ID does not exist");
  $! = ESRCH;
  return;
}

# Resolve a session reference to its corresponding ID.

sub ID_session_to_id {
  my ($self, $session) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> undefined session in ID_session_to_id()"
      unless defined $session;
  };

  my $id = $self->_data_ses_resolve_to_id($session);
  if (defined $id) {
    $! = 0;
    return $id;
  }

  $self->_explain_return("session ($session) does not exist");
  $! = ESRCH;
  return;
}

#==============================================================================
# Extra reference counts, to keep sessions alive when things occur.
# They take session IDs because they may be called from resources at
# times where the session reference is otherwise unknown.
#==============================================================================

sub refcount_increment {
  my ($self, $session_id, $tag) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> undefined session ID in refcount_increment()"
      unless defined $session_id;
    _confess "<us> undefined reference count tag in refcount_increment()"
      unless defined $tag;
  };

  my $session = $self->_data_sid_resolve($session_id);
  unless (defined $session) {
    $self->_explain_return("session id $session_id does not exist");
    $! = ESRCH;
    return;
  }

  my $refcount = $self->_data_extref_inc($session, $tag);
  # TODO trace it here
  return $refcount;
}

sub refcount_decrement {
  my ($self, $session_id, $tag) = @_;

  if (ASSERT_USAGE) {
    _confess "<us> undefined session ID in refcount_decrement()"
      unless defined $session_id;
    _confess "<us> undefined reference count tag in refcount_decrement()"
      unless defined $tag;
  };

  my $session = $self->_data_sid_resolve($session_id);
  unless (defined $session) {
    $self->_explain_return("session id $session_id does not exist");
    $! = ESRCH;
    return;
  }

  my $refcount = $self->_data_extref_dec($session, $tag);

  # We don't need to garbage-test the decremented session if the
  # reference count is nonzero.  Likewise, we don't need to GC it if
  # it's the current session under the assumption that it will be GC
  # tested when the current event dispatch is through.

  if ( !$refcount and $kr_active_session->ID ne $session_id ) {
    $self->_data_ses_collect_garbage($session);
  }

  # TODO trace it here
  return $refcount;
}

#==============================================================================
# HANDLERS
#==============================================================================

# Add or remove event handlers from sessions.
sub state {
  my ($self, $event, $state_code, $state_alias) = @_;
  $state_alias = $event unless defined $state_alias;

  if (ASSERT_USAGE) {
    _confess "<us> must call state() from a running session"
      if $kr_active_session == $self;
    _confess "<us> undefined event name in state()" unless defined $event;
    _confess "<us> can't call state() outside a session" if (
      $kr_active_session == $self
    );
  };

  if (
    (ref($kr_active_session) ne '') &&
    (ref($kr_active_session) ne 'POE::KernelX')
  ) {
    $kr_active_session->_register_state($event, $state_code, $state_alias);
    return 0;
  }

  # TODO A terminal signal (such as UIDESTROY) kills a session.  The
  # Kernel deallocates the session, which cascades destruction to its
  # HEAP.  That triggers a Wheel's destruction, which calls
  # $kernel->state() to remove a state from the session.  The session,
  # though, is already gone.  If TRACE_RETVALS and/or ASSERT_RETVALS
  # is set, this causes a warning or fatal error.

  $self->_explain_return("session ($kr_active_session) does not exist");
  return ESRCH;
}

1;

