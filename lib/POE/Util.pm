package POE::Util;
use strict;

use Sub::Exporter -setup => {
	exports => [qw/
		_make_init_arg
		BUILDARGS
	/]
};

sub _make_init_arg {
	my $init_arg = shift;
	$init_arg =~ s/_(\w)/uc $1/e;
	ucfirst( $init_arg );
};

sub BUILDARGS {
	my $type = shift;

	my $params = {};
	if ( @_==1 && ref $_[0] eq 'HASH' ) { $params = $_[0] }
	elsif ( scalar @_ % 2 ) {
		Carp::croak "$type must be given an even number of parameters";
	}
	else {
		my $params = {@_};
	}
	return $params;
}

1;
