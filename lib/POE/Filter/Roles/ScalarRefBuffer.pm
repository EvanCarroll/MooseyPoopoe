package POE::Filter::Roles::ScalarRefBuffer;
use strict;

use Moose::Role;
use MooseX::AttributeHelpers;
use MooseX::Clone;

use namespace::clean -except => 'meta';

has 'buffer' => (
	isa        => 'ScalarRef'
	, is       => 'rw'
	, default  => sub { \my $buffer; }
	, traits   => [qw(NoClone)]
);

sub get_one_start { ${$_[0]->buffer} .= join '', @{$_[1]} }
  

=for comment

	This was in the old Line.pm's get_one_start, which had a static DEBUG flag 
	DEBUG and do {
    my $temp = join '', @$stream;
    $temp = unpack 'H*', $temp;
    warn "got some raw data: $temp\n";
  };

=cut

sub get_pending {
	my $self = shift;
	
	return defined ${$self->buffer} && length ${$self->buffer}
		? [ ${$self->buffer} ]
		: undef
	;
}

1;
