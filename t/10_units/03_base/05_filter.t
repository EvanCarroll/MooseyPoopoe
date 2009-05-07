#!/usr/bin/perl -w

use strict;

use Test::More tests => 2;

BEGIN { use_ok("POE::Filter") }

eval { my $x = POE::Filter->new() };
ok( $@, "don't instantiate POE::Filter");

