#!/usr/bin/perl -w

use strict;

use Test::More tests => 2;

BEGIN { use_ok("POE::Driver") }

eval { my $x = POE::Driver->new() };
ok( $@, "don't instantiate POE::Driver" );

