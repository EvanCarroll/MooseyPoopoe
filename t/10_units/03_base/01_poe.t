#!/usr/bin/perl -w
# $Id: 01_poe.t 2116 2006-09-08 04:45:45Z rcaputo $
# vim: filetype=perl

use strict;

use Test::More tests => 4;

BEGIN { eval "use POE"; ok(!$@, "you just saved a kitten"); }

# Start with errors.

eval { POE->import( qw( NFA Session ) ) };
ok(
  $@ && $@ =~ /export conflicting constants/,
  "don't import POE::NFA and POE::Session together"
);

open(SAVE_STDERR, ">&STDERR") or die $!;
close(STDERR) or die $!;

eval {
  POE->import( qw( nonexistent ) );
};

open(STDERR, ">&SAVE_STDERR") or die $!;
close(SAVE_STDERR) or die $!;

ok(
  $@ && $@ =~ /could not import qw\(nonexistent\)/,
  "don't import nonexistent modules"
);

eval {POE->import( qw( Loop::Foo Loop::Bar) ) };
ok(
  $@ && $@ =~ /multiple event loops/,
  "don't load more than one event loop"
);

exit 0;
