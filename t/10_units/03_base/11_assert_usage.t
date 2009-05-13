#!/usr/bin/perl -w
# $Id: 11_assert_usage.t 1817 2005-06-29 04:56:32Z rcaputo $
# vim: filetype=perl

# Test the ASSERT_USAGE code in POE::Kernel.  This involves a lot of
# dying.

use strict;
use lib qw(./mylib);

use Test::More tests => 76;

use Symbol qw(gensym);

sub POE::Kernel::ASSERT_USAGE   () { 1 }
sub POE::Kernel::TRACE_REFCNT   () { 1 }
sub POE::Kernel::TRACE_FILENAME () { "./test-output.err" }

BEGIN { use_ok("POE") }

# Disable any "didn't call run" warnings.  We create a bunch of
# sessions, but we're not testing whether they run.  Furthermore, they
# may leave alarms or filehandles selected, which could cause the
# program to hang if we DO try to run it.

POE::Kernel->run();

# Test usage outside a running session.

foreach my $method (
  qw(
    alarm alarm_add alarm_adjust alarm_remove alarm_remove_all
    alarm_set delay delay_add delay_adjust delay_set detach_child
    detach_myself select select_expedite select_pause_read
    select_pause_write select_read select_resume_read
    select_resume_write select_write sig state yield
  )
) {
  my $message = "must call $method() from a running session";
  eval { $poe_kernel->$method() };
  like( $@, qr/\Q$message/, $message );
}

# Signal functions.

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->sig(undef) };
      like($@, qr/undefined signal in sig/, "undefined signal assertion");

      eval { $poe_kernel->signal(undef) };
      like(
        $@, qr/undefined destination in signal/,
        "undefined destination in signal"
      );

      eval { $poe_kernel->signal($poe_kernel, undef) };
      like(
        $@, qr/undefined signal in signal/,
        "undefined signal in signal"
      );
    }
  }
);

# Internal _dispatch_event() function.

# TODO - Determine whether it  needs ASSERT_USAGE checks.

# Post, yield, call.

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->post(undef) };
      like(
        $@, qr/destination is undefined in post/,
        "destination undefined in post"
      );

      eval { $poe_kernel->post($poe_kernel, undef) };
      like(
        $@, qr/event is undefined in post/,
        "event undefined in post"
      );

      eval { $poe_kernel->yield(undef) };
      like(
        $@, qr/event name is undefined in yield/,
        "event undefined in yield"
      );

      eval { $poe_kernel->call(undef) };
      like(
        $@, qr/destination is undefined in call/,
        "destination undefined in call"
      );

      eval { $poe_kernel->call($poe_kernel, undef) };
      like(
        $@, qr/event is undefined in call/,
        "event undefined in call"
      );
    }
  }
);

# Classic alarms.

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->alarm(undef) };
      like(
        $@, qr/event name is undefined in alarm/,
        "event undefined in alarm"
      );

      eval { $poe_kernel->alarm_add(undef) };
      like(
        $@, qr/undefined event name in alarm_add/,
        "event undefined in alarm_add"
      );

      eval { $poe_kernel->alarm_add(moo => undef) };
      like(
        $@, qr/undefined time in alarm_add/,
        "time undefined in alarm_add"
      );

      eval { $poe_kernel->delay(undef) };
      like(
        $@, qr/undefined event name in delay/,
        "event undefined in delay"
      );

      eval { $poe_kernel->delay_add(undef) };
      like(
        $@, qr/undefined event name in delay_add/,
        "event undefined in delay_add"
      );

      eval { $poe_kernel->delay_add(moo => undef) };
      like(
        $@, qr/undefined time in delay_add/,
        "time undefined in delay_add"
      );
    }
  }
);

# New alarms.

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->alarm_set(undef) };
      like(
        $@, qr/undefined event name in alarm_set/,
        "event undefined in alarm_set"
      );

      eval { $poe_kernel->alarm_set(moo => undef) };
      like(
        $@, qr/undefined time in alarm_set/,
        "time undefined in alarm_set"
      );

      eval { $poe_kernel->alarm_remove(undef) };
      like(
        $@, qr/undefined alarm id in alarm_remove/,
        "alarm ID undefined in alarm_remove"
      );

      eval { $poe_kernel->alarm_adjust(undef) };
      like(
        $@, qr/undefined alarm id in alarm_adjust/,
        "alarm ID undefined in alarm_adjust"
      );

      eval { $poe_kernel->alarm_adjust(moo => undef) };
      like(
        $@, qr/undefined alarm delta in alarm_adjust/,
        "alarm time undefined in alarm_adjust"
      );

      eval { $poe_kernel->delay_set(undef) };
      like(
        $@, qr/undefined event name in delay_set/,
        "event name undefined in delay_set"
      );

      eval { $poe_kernel->delay_set(moo => undef) };
      like(
        $@, qr/undefined seconds in delay_set/,
        "seconds undefined in delay_set"
      );

      eval { $poe_kernel->delay_adjust(undef) };
      like(
        $@, qr/undefined delay id in delay_adjust/,
        "delay ID undefined in delay_adjust"
      );

      eval { $poe_kernel->delay_adjust(moo => undef) };
      like(
        $@, qr/undefined delay seconds in delay_adjust/,
        "delay seconds undefined in delay_adjust"
      );
    }
  }
);

POE::Session->create(
  inline_states => {
    _start => sub {
      $_[KERNEL]->alias_set("moo");
    }
  }
);

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $_[KERNEL]->alias_set("moo") };
      like(
        $@, qr/alias 'moo' is in use by another session/,
        "alias already in use"
      );

      eval { $_[KERNEL]->alias_remove("moo") };
      like(
        $@, qr/alias does not belong to current session/,
        "alias belongs to another session"
      );
    }
  }
);

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->alias_remove("narf") };
      like(
        $@, qr/alias does not exist/,
        "alias does not exist"
      );
    }
  }
);

# Filehandle I/O.

POE::Session->create(
  inline_states => {
    _start => sub {
      my $fh = gensym();

      eval { $poe_kernel->select(undef) };
      like(
        $@, qr/undefined filehandle in select/,
        "filehandle undefined in select"
      );

      eval { $poe_kernel->select($fh) };
      like(
        $@, qr/invalid filehandle in select/,
        "filehandle closed in select"
      );

      eval { $poe_kernel->select_read(undef) };
      like(
        $@, qr/undefined filehandle in select_read/,
        "filehandle undefined in select_read"
      );

      eval { $poe_kernel->select_read($fh) };
      like(
        $@, qr/invalid filehandle in select_read/,
        "filehandle closed in select_read"
      );

      eval { $poe_kernel->select_write(undef) };
      like(
        $@, qr/undefined filehandle in select_write/,
        "filehandle undefined in select_write"
      );

      eval { $poe_kernel->select_write($fh) };
      like(
        $@, qr/invalid filehandle in select_write/,
        "filehandle closed in select_write"
      );

      eval { $poe_kernel->select_expedite(undef) };
      like(
        $@, qr/undefined filehandle in select_expedite/,
        "filehandle undefined in select_expedite"
      );

      eval { $poe_kernel->select_expedite($fh) };
      like(
        $@, qr/invalid filehandle in select_expedite/,
        "filehandle closed in select_expedite"
      );

      eval { $poe_kernel->select_pause_write(undef) };
      like(
        $@, qr/undefined filehandle in select_pause_write/,
        "filehandle undefined in select_pause_write"
      );

      eval { $poe_kernel->select_pause_write($fh) };
      like(
        $@, qr/invalid filehandle in select_pause_write/,
        "filehandle closed in select_pause_write"
      );

      eval { $poe_kernel->select_resume_write(undef) };
      like(
        $@, qr/undefined filehandle in select_resume_write/,
        "filehandle undefined in select_resume_write"
      );

      eval { $poe_kernel->select_resume_write($fh) };
      like(
        $@, qr/invalid filehandle in select_resume_write/,
        "filehandle closed in select_resume_write"
      );

      eval { $poe_kernel->select_pause_read(undef) };
      like(
        $@, qr/undefined filehandle in select_pause_read/,
        "filehandle undefined in select_pause_read"
      );

      eval { $poe_kernel->select_pause_read($fh) };
      like(
        $@, qr/invalid filehandle in select_pause_read/,
        "filehandle closed in select_pause_read"
      );

      eval { $poe_kernel->select_resume_read(undef) };
      like(
        $@, qr/undefined filehandle in select_resume_read/,
        "filehandle undefined in select_resume_read"
      );

      eval { $poe_kernel->select_resume_read($fh) };
      like(
        $@, qr/invalid filehandle in select_resume_read/,
        "filehandle closed in select_resume_read"
      );
    }
  }
);

# Aliases.

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->alias_set(undef) };
      like(
        $@, qr/undefined alias in alias_set/,
        "undefined alias in alias_set"
      );

      eval { $poe_kernel->alias_remove(undef) };
      like(
        $@, qr/undefined alias in alias_remove/,
        "undefined alias in alias_remove"
      );

      eval { $poe_kernel->alias_resolve(undef) };
      like(
        $@, qr/undefined alias in alias_resolve/,
        "undefined alias in alias_resolve"
      );
    }
  }
);

# Kernel and session IDs.

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->ID_id_to_session(undef) };
      like(
        $@, qr/undefined ID in ID_id_to_session/,
        "undefined ID in ID_id_to_session"
      );

      eval { $poe_kernel->ID_session_to_id(undef) };
      like(
        $@, qr/undefined session in ID_session_to_id/,
        "undefined session in ID_session_to_id"
      );
    }
  }
);

# Extra references.

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->refcount_increment(undef) };
      like(
        $@, qr/undefined session ID in refcount_increment/,
        "undefined session ID in refcount_increment"
      );

      eval { $poe_kernel->refcount_increment("moo", undef) };
      like(
        $@, qr/undefined reference count tag in refcount_increment/,
        "undefined tag in refcount_increment"
      );

      eval { $poe_kernel->refcount_decrement(undef) };
      like(
        $@, qr/undefined session ID in refcount_decrement/,
        "undefined session ID in refcount_decrement"
      );

      eval { $poe_kernel->refcount_decrement("moo", undef) };
      like(
        $@, qr/undefined reference count tag in refcount_decrement/,
        "undefined tag in refcount_decrement"
      );
    }
  }
);

# Event handlers.

POE::Session->create(
  inline_states => {
    _start => sub {
      eval { $poe_kernel->state(undef) };
      like(
        $@, qr/undefined event name in state/,
        "undefined event name in state"
      );
    }
  }
);

exit 0;
