package POE::Filter::Roles::ScalarBuffer;
use strict;

use Moose::Role;
use MooseX::AttributeHelpers;

use namespace::clean -except => 'meta';

has 'buffer' => (
	isa        => 'Str'
	, is       => 'rw'
	, default  => ''
	, metaclass => 'String'

	, provides  => {
		append  => 'append_to_buffer'
		, clear    => 'clear_buffer'
		, 'substr' => 'substr_buffer'
	}

);

sub get_one_start { $_[0]->append_to_buffer( join '', @{$_[1]} ); }

sub get_pending {
	my $self = shift;
	
	return defined $self->buffer && length $self->buffer
		? [ $self->buffer ]
		: undef
	;
}

1;
