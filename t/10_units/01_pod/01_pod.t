# $Id: 01_pod.t 2136 2006-09-19 22:00:21Z apocal $
# vim: filetype=perl

use Test::More;
eval "use Test::Pod 1.00";
plan skip_all => "Test::Pod 1.00 required for testing POD" if $@;
all_pod_files_ok();
