#!/usr/bin/perl -w
# $Id: gen-meta.perl 2389 2008-07-05 20:15:25Z rcaputo $
# rocco // vim: ts=2 sw=2 expandtab

# Generate META.yml.

use strict;
use lib qw(./mylib);

use Module::Build;
use PoeBuildInfo qw(
  CORE_REQUIREMENTS
  DIST_ABSTRACT
  DIST_AUTHOR
  RECOMMENDED_TIME_HIRES
  CONFIG_REQUIREMENTS
);

my $build = Module::Build->new(
  dist_abstract     => DIST_ABSTRACT,
  dist_author       => DIST_AUTHOR,
  dist_name         => 'POE',
  dist_version_from => 'lib/POE.pm',
  license           => 'perl',
  recommends        => {
    RECOMMENDED_TIME_HIRES,
  },
  requires          => { CORE_REQUIREMENTS },
  build_requires    => { CONFIG_REQUIREMENTS },
  configure_requires => { CONFIG_REQUIREMENTS },
  no_index => {
    directory => [ "mylib", "t" ]
  },
);

$build->dispatch("distmeta");

exit;
