package POE::Resource::Sessions;
use Moose::Role;
use strict;

use MooseX::AttributeHelpers;

has 'kr_sessions' => (
	isa  => 'HashRef'
	, is => 'ro'
	, default   => sub { +{} }
	, metaclass => 'Collection::Hash'
	, provides  => {
		### Determine whether a session exists.  We should only need to verify
		### this for sessions provided by the outside.  Internally, our code
		### should be so clean it's not necessary.
		'exists' => '_data_ses_exists'
		, count  => '_data_ses_count'
	}
);

use constant {
	SS_SESSION      => 0
	, SS_REFCOUNT   => 1
	, SS_PARENT     => 2
	, SS_CHILDREN   => 3
	, SS_PROCESSES  => 4
	, SS_ID         => 5
};

sub _data_ses_finalize {
	my $kr_sessions = $_[0]->kr_sessions;
  my $finalized_ok = 1;

  while (my ($ses, $ses_rec) = each(%$kr_sessions)) {
    $finalized_ok = 0;

    POE::Kernel::_warn(
      "!!! Leaked session: $ses\n",
      "!!!\trefcnt = $ses_rec->[SS_REFCOUNT]\n",
      "!!!\tparent = $ses_rec->[SS_PARENT]\n",
      "!!!\tchilds = ", join("; ", keys(%{$ses_rec->[SS_CHILDREN]})), "\n",
      "!!!\tprocs  = ", join("; ", keys(%{$ses_rec->[SS_PROCESSES]})),"\n",
    );
  }

  return $finalized_ok;
}

### Enter a new session into the back-end stuff.

sub _data_ses_allocate {
  my ($self, $session, $sid, $parent) = @_;
	my $kr_sessions = $self->kr_sessions;

  if (POE::Kernel::ASSERT_DATA and defined($parent)) {
    POE::Kernel::_trap "parent $parent does not exist"
      unless exists $kr_sessions->{$parent};
    POE::Kernel::_trap "session $session is already allocated"
      if exists $kr_sessions->{$session};
  }

  $kr_sessions->{$session} =
    [ $session,  # SS_SESSION
      0,         # SS_REFCOUNT
      $parent,   # SS_PARENT
      { },       # SS_CHILDREN
      { },       # SS_PROCESSES
      $sid,      # SS_ID
    ];

  # For the ID to session reference lookup.
  $self->_data_sid_set($sid, $session);

  # Manage parent/child relationship.
  if (defined $parent) {
    if (POE::Kernel::TRACE_SESSIONS) {
      POE::Kernel::_warn(
        "<ss> ",
        $self->_data_alias_loggable($session), " has parent ",
        $self->_data_alias_loggable($parent)
      );
    }

    $kr_sessions->{$parent}->[SS_CHILDREN]->{$session} = $session;
    $self->_data_ses_refcount_inc($parent);
  }
}

# Release a session's resources, and remove it.  This doesn't do
# garbage collection for the session itself because that should
# already have happened.
#
# TODO This is yet another place where resources will need to register
# a function.  Every resource's _data_???_clear_session is called
# here.

sub _data_ses_free {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;

  if (POE::Kernel::TRACE_SESSIONS) {
    POE::Kernel::_warn(
      "<ss> freeing ",
      $self->_data_alias_loggable($session)
    );
  }

  # Manage parent/child relationships.

  my $parent = $kr_sessions->{$session}->[SS_PARENT];
  my @children = $self->_data_ses_get_children($session);

  if (defined $parent) {
    if (POE::Kernel::ASSERT_DATA) {
      POE::Kernel::_trap "session is its own parent"
        if $parent == $session;
      POE::Kernel::_trap "session's parent ($parent) doesn't exist"
        unless exists $kr_sessions->{$parent};

      unless ($self->_data_ses_is_child($parent, $session)) {
        POE::Kernel::_trap(
          $self->_data_alias_loggable($session), " isn't a child of ",
          $self->_data_alias_loggable($parent), " (it's a child of ",
          $self->_data_alias_loggable($self->_data_ses_get_parent($session)),
          ")"
        );
      }
    }

    # Remove the departing session from its parent.

    POE::Kernel::_trap "internal inconsistency ($parent/$session)"
      unless delete $kr_sessions->{$parent}->[SS_CHILDREN]->{$session};

    undef $kr_sessions->{$session}->[SS_PARENT];

    if (POE::Kernel::TRACE_SESSIONS) {
      POE::Kernel::_cluck(
        "<ss> removed ",
        $self->_data_alias_loggable($session), " from ",
        $self->_data_alias_loggable($parent)
      );
    }

    $self->_data_ses_refcount_dec($parent);

    # Move the departing session's children to its parent.

    foreach (@children) {
      $self->_data_ses_move_child($_, $parent)
    }
  }
  elsif (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap "no parent to give children to" if @children;
  }

  # Things which do not hold reference counts.

  $self->_data_sid_clear($session);            # Remove from SID tables.
  $self->_data_sig_clear_session($session);    # Remove all leftover signals.

  # Things which do hold reference counts.

  $self->_data_alias_clear_session($session);  # Remove all leftover aliases.
  $self->_data_extref_clear_session($session); # Remove all leftover extrefs.
  $self->_data_handle_clear_session($session); # Remove all leftover handles.
  $self->_data_ev_clear_session($session);     # Remove all leftover events.

  if (POE::Kernel::TRACE_PROFILE) {
    $self->_data_stat_clear_session($session);
  }

  # Remove the session itself.

  delete $kr_sessions->{$session};
}

### Move a session to a new parent.

sub _data_ses_move_child {
  my ($self, $session, $new_parent) = @_;
	my $kr_sessions = $self->kr_sessions;

  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("moving nonexistent child to another parent")
      unless exists $kr_sessions->{$session};
    POE::Kernel::_trap("moving child to a nonexistent parent")
      unless exists $kr_sessions->{$new_parent};
  }

  if (POE::Kernel::TRACE_SESSIONS) {
    POE::Kernel::_warn(
      "<ss> moving ",
      $self->_data_alias_loggable($session), " to ",
      $self->_data_alias_loggable($new_parent)
    );
  }

  my $old_parent = $self->_data_ses_get_parent($session);

  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("moving child from a nonexistent parent")
      unless exists $kr_sessions->{$old_parent};
  }

  # Remove the session from its old parent.
  delete $kr_sessions->{$old_parent}->[SS_CHILDREN]->{$session};

  if (POE::Kernel::TRACE_SESSIONS) {
    POE::Kernel::_warn(
      "<ss> removed ",
      $self->_data_alias_loggable($session), " from ",
      $self->_data_alias_loggable($old_parent)
    );
  }

  $self->_data_ses_refcount_dec($old_parent);

  # Change the session's parent.
  $kr_sessions->{$session}->[SS_PARENT] = $new_parent;

  if (POE::Kernel::TRACE_SESSIONS) {
    POE::Kernel::_warn(
      "<ss> changed parent of ",
      $self->_data_alias_loggable($session), " to ",
      $self->_data_alias_loggable($new_parent)
    );
  }

  # Add the current session to the new parent's children.
  $kr_sessions->{$new_parent}->[SS_CHILDREN]->{$session} = $session;

  if (POE::Kernel::TRACE_SESSIONS) {
    POE::Kernel::_warn(
      "<ss> added ",
      $self->_data_alias_loggable($session), " as child of ",
      $self->_data_alias_loggable($new_parent)
    );
  }

  $self->_data_ses_refcount_inc($new_parent);

  # We do not call _data_ses_collect_garbage() here.  This function is
  # called in batch for a departing session, to move its children to
  # its parent.  The GC test would be superfluous here.  Rather, it's
  # up to the caller to do the proper GC test after moving things
  # around.
}

### Get a session's parent.

sub _data_ses_get_parent {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;
  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("retrieving parent of a nonexistent session")
      unless exists $kr_sessions->{$session};
  }
  return $kr_sessions->{$session}->[SS_PARENT];
}

### Get a session's children.

sub _data_ses_get_children {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;
  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("retrieving children of a nonexistent session")
      unless exists $kr_sessions->{$session};
  }
  return values %{$kr_sessions->{$session}->[SS_CHILDREN]};
}

### Is a session a child of another?

sub _data_ses_is_child {
  my ($self, $parent, $child) = @_;
	my $kr_sessions = $self->kr_sessions;

  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("testing is-child of a nonexistent parent session")
      unless exists $kr_sessions->{$parent};
  }
  return exists $kr_sessions->{$parent}->[SS_CHILDREN]->{$child};
}

### Resolve a session into its reference.
sub _data_ses_resolve {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;

  return undef unless exists $kr_sessions->{$session}; # Prevents autoviv.
  return $kr_sessions->{$session}->[SS_SESSION];
}

### Resolve a session ID into its reference.
sub _data_ses_resolve_to_id {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;
  return undef unless exists $kr_sessions->{$session}; # Prevents autoviv.
  return $kr_sessions->{$session}->[SS_ID];
}

### Decrement a session's main reference count.  This is called by
### each watcher when the last thing it watches for the session goes
### away.  In other words, a session's reference count should only
### enumerate the different types of things being watched; not the
### number of each.

sub _data_ses_refcount_dec {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;

  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("decrementing refcount of a nonexistent session")
      unless exists $kr_sessions->{$session};
  }

  if (POE::Kernel::TRACE_REFCNT) {
    POE::Kernel::_warn(
      "<rc> decrementing refcount for ",
      $self->_data_alias_loggable($session)
    );
  }

  $kr_sessions->{$session}->[SS_REFCOUNT]--;

  if (POE::Kernel::ASSERT_DATA and $kr_sessions->{$session}->[SS_REFCOUNT] < 0) {
    POE::Kernel::_trap(
      $self->_data_alias_loggable($session),
     " reference count went below zero"
   );
  }
}

### Increment a session's main reference count.

sub _data_ses_refcount_inc {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;

  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("incrementing refcount for nonexistent session")
      unless exists $kr_sessions->{$session};
  }

  if (POE::Kernel::TRACE_REFCNT) {
    POE::Kernel::_warn(
      "<rc> incrementing refcount for ",
      $self->_data_alias_loggable($session)
    );
  }

  $kr_sessions->{$session}->[SS_REFCOUNT]++;
}

# Query a session's reference count.  Added for testing purposes.

sub _data_ses_refcount {
  return $_[0]->kr_sessions->{$_[1]}->[SS_REFCOUNT];
}

### Determine whether a session is ready to be garbage collected.
### Free the session if it is.

sub _data_ses_collect_garbage {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;

  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("collecting garbage for a nonexistent session")
      unless exists $kr_sessions->{$session};
  }

  if (POE::Kernel::TRACE_REFCNT) {
    my $ss = $kr_sessions->{$session};
    POE::Kernel::_warn(
      "<rc> +----- GC test for ", $self->_data_alias_loggable($session),
      " ($session) -----\n",
      "<rc> | total refcnt  : ", $ss->[SS_REFCOUNT], "\n",
      "<rc> | event count   : ", $self->_data_ev_get_count_to($session), "\n",
      "<rc> | post count    : ", $self->_data_ev_get_count_from($session), "\n",
      "<rc> | child sessions: ", scalar(keys(%{$ss->[SS_CHILDREN]})), "\n",
      "<rc> | handles in use: ", $self->_data_handle_count_ses($session), "\n",
      "<rc> | aliases in use: ", $self->_data_alias_count_ses($session), "\n",
      "<rc> | extra refs    : ", $self->_data_extref_count_ses($session), "\n",
      "<rc> | pid count     : ", $self->_data_sig_pids_ses($session), "\n",
      "<rc> +---------------------------------------------------\n",
    );
    unless ($ss->[SS_REFCOUNT]) {
      POE::Kernel::_warn(
        "<rc> | ", $self->_data_alias_loggable($session),
        " is garbage; stopping it...\n",
        "<rc> +---------------------------------------------------\n",
      );
    }
    POE::Kernel::_carp "<rc> | called";
  }

  if (POE::Kernel::ASSERT_DATA) {
    my $ss = $kr_sessions->{$session};
    my $calc_ref = (
      $self->_data_ev_get_count_to($session) +
      $self->_data_ev_get_count_from($session) +
      scalar(keys(%{$ss->[SS_CHILDREN]})) +
      $self->_data_handle_count_ses($session) +
      $self->_data_extref_count_ses($session) +
      $self->_data_alias_count_ses($session) +
      $self->_data_sig_pids_ses($session)
    );

    # The calculated reference count really ought to match the one
    # POE's been keeping track of all along.

    POE::Kernel::_trap(
      "<dt> ", $self->_data_alias_loggable($session),
       " has a reference count inconsistency",
       " (calc=$calc_ref; actual=$ss->[SS_REFCOUNT])\n"
     ) if $calc_ref != $ss->[SS_REFCOUNT];
  }

  return if $kr_sessions->{$session}->[SS_REFCOUNT];

  $self->_data_ses_stop($session);
}


### Close down a session by force.

# Stop a session, dispatching _stop, _parent, and _child as necessary.
#
# Dispatch _stop to a session, removing it from the kernel's data
# structures as a side effect.

my %already_stopping;

sub _data_ses_stop {
  my ($self, $session) = @_;
	my $kr_sessions = $self->kr_sessions;

  # Don't stop a session that's already in the throes of stopping.
  # This can happen with exceptions, during die() in _stop.  It can
  # probably be removed if exceptions are.
  return if exists $already_stopping{$session};
  $already_stopping{$session} = 1;

  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("stopping a nonexistent session")
      unless exists $kr_sessions->{$session};
  }

  if (POE::Kernel::TRACE_SESSIONS) {
    POE::Kernel::_warn("<ss> stopping ", $self->_data_alias_loggable($session));
  }

  # Maintain referential integrity between parents and children.
  # First move the children of the stopping session up to its parent.
  my $parent = $self->_data_ses_get_parent($session);

  foreach my $child ($self->_data_ses_get_children($session)) {
    $self->_dispatch_event(
      $parent, $self,
      POE::Kernel::EN_CHILD, POE::Kernel::ET_CHILD, [ POE::Kernel::CHILD_GAIN, $child ],
      __FILE__, __LINE__, undef, time(), -__LINE__
    );
    $self->_dispatch_event(
      $child, $self,
      POE::Kernel::EN_PARENT, POE::Kernel::ET_PARENT,
      [ $self->_data_ses_get_parent($child), $parent, ],
      __FILE__, __LINE__, undef, time(), -__LINE__
    );
  }

  # Referential integrity has been dealt with.  Now notify the session
  # that it has been stopped.
  my $stop_return = $self->_dispatch_event(
    $session, $self->get_active_session(),
    POE::Kernel::EN_STOP, POE::Kernel::ET_STOP, [],
    __FILE__, __LINE__, undef, time(), -__LINE__
  );

  # If the departing session has a parent, notify it that the session
  # is being lost.

  if (defined $parent) {
    $self->_dispatch_event(
      $parent, $self,
      POE::Kernel::EN_CHILD, POE::Kernel::ET_CHILD, [ POE::Kernel::CHILD_LOSE, $session, $stop_return ],
      __FILE__, __LINE__, undef, time(), -__LINE__
    );
  }

  # Deallocate the session.
  $self->_data_ses_free($session);

  # GC the parent, if there is one.
  if (defined $parent and $parent != $self) {
    $self->_data_ses_collect_garbage($parent);
  }

  # Stop the main loop if everything is gone.
  # XXX - Under Tk this is called twice.  Why?  WHY is it called twice?
  unless (keys %$kr_sessions) {
    $self->loop_halt();
  }

  delete $already_stopping{$session};
}


1;

__END__

=head1 NAME

POE::Resource::Sessions - internal session manager for POE::Kernel

=head1 SYNOPSIS

There is no public API.

=head1 DESCRIPTION

POE::Resource::Sessions is a mix-in class for POE::Kernel.  It
provides the internal features that manage sessions, regardless of the
session type.  It is used internally by POE::Kernel. so it has no
public interface.

=head1 SEE ALSO

See L<POE::Session> and L<POE::NFA> for one type of session.  CPAN
also have others.

See L<POE::Kernel/Sessions> for a discussion about POE::Kernel
sessions.

See L<POE::Kernel/Session Lifespans> to learn why sessions run, and
how to stop them.

See L<POE::Kernel/Session Management> for information about managing
sessions in your applications, and the events that occur when sessions
come and go.

See L<POE::Kernel/Session Helper Methods> for friend methods between
POE::Kernel and POE::Session classes.

See L<POE::Kernel/Resources> for for public information about POE
resources.

See L<POE::Resource> for general discussion about resources and the
classes that manage them.

=head1 BUGS

None known.

=head1 AUTHORS & COPYRIGHTS

Please see L<POE> for more information about authors and contributors.

=cut

# rocco // vim: ts=2 sw=2 expandtab
# TODO - Edit.
