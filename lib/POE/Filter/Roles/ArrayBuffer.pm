package POE::Filter::Roles::ArrayBuffer;
use strict;

use Moose::Role;

use namespace::clean -except => 'meta';

has 'buffer' => (
	isa  => 'ArrayRef'
	, is => 'ro'
	, default => sub { +[] }
);

sub get_one_start {
	my ($self, $data) = @_;
	push @{$self->buffer}, @$data if defined $data;
}

sub get_pending {
	my $self = shift;
	return scalar @{$self->buffer}
		? [ @{$self->buffer} ]
		: undef
	;
}

1;
