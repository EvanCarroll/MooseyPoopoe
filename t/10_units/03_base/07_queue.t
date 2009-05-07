#!/usr/bin/perl -w

use strict;

use Test::More tests => 2;

BEGIN { use_ok("POE::Queue") }

eval { my $x = POE::Queue->new() };
ok( $@, "don't instantiate POE::Queue" );

