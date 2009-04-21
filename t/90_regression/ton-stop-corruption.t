#!/usr/bin/perl -w
# $Id: ton-stop-corruption.t 1625 2004-09-16 01:38:17Z rcaputo $

# Test that stop() does not result in a double garbage collection on
# the session that called it.  This test case provided by Ton Hospel.

use strict;

use Test::More tests => 5;

sub POE::Kernel::ASSERT_DEFAULT () { 1 }

BEGIN { use_ok("POE") }
BEGIN { use_ok("POE::Pipe::OneWay") }

BEGIN { $^W = 1 };

my ($rd, $wr) = POE::Pipe::OneWay->new();
ok(defined($rd), "created a pipe for testing ($!)");

my $stop_was_called = 0;

POE::Session->create(
  inline_states => {
    _start       => sub {
      $poe_kernel->select_read($rd, "readable");
    },
    readable     => sub {
      pass("got readable callback; calling stop");
      $poe_kernel->select_read($rd);
      $poe_kernel->stop();
    },
    _stop   => sub { $stop_was_called++ },
    _parent => sub { },
    _child  => sub { },
  }
);

close $wr;

POE::Kernel->run();

ok( !$stop_was_called, "stop was not called" );

exit;
