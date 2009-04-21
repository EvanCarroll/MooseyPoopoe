#!/usr/bin/perl -w
# $Id: averell-callback-ret.t 1826 2005-07-25 02:12:25Z rcaputo $

# Callback must pass on it's return value as per documentation.

use strict;

use Test::More tests => 2;

sub POE::Kernel::ASSERT_DEFAULT () { 1 }

BEGIN { use_ok("POE") } #1

BEGIN { $^W = 1 };

POE::Session->create(
  inline_states => {
    _start       => sub {
      $_[HEAP]->{callback} = $_[SESSION]->callback("callback_event");
      $_[KERNEL]->yield('try_callback');
    },
    try_callback => sub {
      my $callback = delete $_[HEAP]->{callback};
      my $retval = $callback->();
      if ($retval == 42) {
        pass("Callback returns correct value"); #2
      } else {
        diag("Callback returned $retval (should be 42)");
        fail("Callback returns correct value");
      }
    },
    callback_event => sub { return 42 },
    _stop => sub {},
  }
);

POE::Kernel->run();

exit;
