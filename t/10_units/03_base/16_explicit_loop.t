#!/usr/bin/perl -w
# $Id: 16_explicit_loop.t 2126 2006-09-16 05:33:53Z rcaputo $

use strict;

use Test::More tests => 1;
sub POE::Kernel::ASSERT_DEFAULT () { 1 }
BEGIN { use_ok("POE", "Loop::Select") }
