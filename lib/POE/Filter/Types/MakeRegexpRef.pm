package POE::Filter::Types::MakeRegexpRef;

use MooseX::Types -declare => [qw(MakeRegexpRef)];
use MooseX::Types::Moose qw(RegexpRef Str);

subtype MakeRegexpRef
	, as RegexpRef
;

coerce MakeRegexpRef
	, from Str
	, via { qr/$_/ }
;

1;

__END__

## PLEASE TELL ME IF YOU FIND THIS TYPE ON CPAN
## IT IS NOT POE SPECIFIC
