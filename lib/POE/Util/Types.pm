package POE::Util::Types;

use MooseX::Types -declare => [qw/Natural/];
use strict;

subtype Natural
	, as 'Int'
	, where { int($_) > 0 }
;
