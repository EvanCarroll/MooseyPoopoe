package POE::Filter::Roles::CodeGetAndPut;
use Moose::Role;
use strict;

use Carp qw(croak carp);

use namespace::clean -except => 'meta';

## XXX Above my normal gripes this module wins the notable award for caps stupidity
##     Get and Put and Code are the names of the coderefs
##     get/put are also sub names - EC

has 'buffer' => ( isa => 'ArrayRef' , is => 'ro' , default => sub { +[] } );

has 'Code' => ( isa => 'CodeRef', is => 'rw' );

foreach ( qw/Get Put/ ) {
	has( $_ => (
		isa         => 'CodeRef'
		, is        => 'rw'
		, predicate => "has_$_"
		, trigger   => \&_get_put_trigger
		, lazy      => 1
		, default   => sub {
				$_[0]->Code || die 'Not a valid Put and Get, or a valid Code'
			}
	) );
};

sub _get_put_trigger {
	my $self = shift;
	Carp::croak 'Both a Get and Put parameter must be present, if either one is'
		unless defined $self->has_Get && defined $self->has_Put
	;
};

sub get_one_start {
	my ($self, $stream) = @_;
	push @{$self->buffer}, @$stream
		if defined $stream
	;
}

sub get_pending {
	my $self = shift;
	return undef unless @{$self->buffer};
	[ @{$self->buffer} ];
}

sub modify {
	my ($self, %params) = @_;

	for (keys %params) {
		unless (ref $params{$_} eq 'CODE') {
			carp("Modify $_ element must be given a coderef");
			next;
		}
		if (lc eq 'code') {
			$self->Get( $params{$_} );
			$self->Put( $params{$_} );
		}
		elsif (lc eq 'put') {
			$self->Put( $params{$_} );
		}
		elsif (lc eq 'get') {
			$self->Get( $params{$_} );
		}
	}
}

1;
