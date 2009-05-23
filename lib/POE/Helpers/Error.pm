package POE::Helpers::Error;
use Carp qw();

use Sub::Exporter -setup => {
	exports => [qw/
		_trap_death _release_death _trap
		_confess _carp _croak _cluck
		_warn _die
	/]
	, groups => {
		Perl => [qw/_warn _die/]
		, Carp => [qw/_confess _carp _croak _cluck/]
		, LogCapture => [qw/_trap_death _release_death/]
	}
};

## TODO: I believe much of this can be replaced with IO::CaptureOutput

#------------------------------------------------------------------------------
# Helpers to carp, croak, confess, cluck, warn and die with whatever
# trace file we're using today.  _trap is reserved for internal
# errors.

BEGIN {
	no strict 'refs';
	if (defined $ENV{POE_TRACE_FILENAME}) {
		open TRACE_FILE, '>', $ENV{POE_TRACE_FILENAME}
			or die "can't open trace file `".$ENV{POE_TRACE_FILENAME}."': $!";
		CORE::select((CORE::select(TRACE_FILE), $| = 1)[0]);
	}
	else {
		*TRACE_FILE = *STDERR;
	}
}

# This block abstracts away a particular piece of voodoo, since we're about
# to call it many times. This is all a big closure around the following two
# variables, allowing us to swap out and replace handlers without the need
# for mucking up the namespace or the kernel itself.
my ($orig_warn_handler, $orig_die_handler);

# _trap_death replaces the current __WARN__ and __DIE__ handlers
# with our own.  We keep the defaults around so we can put them back
# when we're done.  Specifically this is necessary, it seems, for
# older perls that don't respect the C<local *STDERR = *TRACE_FILE>.
#
# TODO - The __DIE__ handler generates a double message if
# TRACE_FILE is STDERR and the die isn't caught by eval.  That's
# messy and needs to go.
sub _trap_death {
	$orig_warn_handler = $SIG{__WARN__};
	$orig_die_handler = $SIG{__DIE__};

	$SIG{__WARN__} = sub { print TRACE_FILE $_[0] };
	$SIG{__DIE__} = sub { print TRACE_FILE $_[0]; die $_[0]; };
}

# _release_death puts the original __WARN__ and __DIE__ handlers back in
# place. Hopefully this is zero-impact camping. The hope is that we can
# do our trace magic without impacting anyone else.
sub _release_death {
	$SIG{__WARN__} = $orig_warn_handler;
	$SIG{__DIE__} = $orig_die_handler;
}

sub _trap {
	local $Carp::CarpLevel = $Carp::CarpLevel + 1;
	local *STDERR = *TRACE_FILE;

	_trap_death();
	Carp::confess(
		"-----\n",
		"Please address any warnings or errors above this message, and try\n",
		"again.  If there are none, or those messages are from within POE,\n",
		"then please mail them along with the following information\n",
		"to bug-POE\@rt.cpan.org:\n---\n@_\n-----\n"
	);
	_release_death();
}

sub _croak {
	local $Carp::CarpLevel = $Carp::CarpLevel + 1;
	local *STDERR = *TRACE_FILE;

	_trap_death();
	Carp::croak(@_);
	_release_death();
}

sub _confess {
	local $Carp::CarpLevel = $Carp::CarpLevel + 1;
	local *STDERR = *TRACE_FILE;

	_trap_death();
	Carp::confess(@_);
	_release_death();
}

sub _cluck {
	local $Carp::CarpLevel = $Carp::CarpLevel + 1;
	local *STDERR = *TRACE_FILE;

	_trap_death();
	Carp::cluck(@_);
	_release_death();
}

sub _carp {
	local $Carp::CarpLevel = $Carp::CarpLevel + 1;
	local *STDERR = *TRACE_FILE;

	_trap_death();
	Carp::carp(@_);
	_release_death();
}

sub _warn {
	my ($package, $file, $line) = caller();
	my $message = join("", @_);
	$message .= " at $file line $line\n" unless $message =~ /\n$/;

	_trap_death();
	warn $message;
	_release_death();
}

sub _die {
	my ($package, $file, $line) = caller();
	my $message = join("", @_);
	$message .= " at $file line $line\n" unless $message =~ /\n$/;
	local *STDERR = *TRACE_FILE;

	_trap_death();
	die $message;
	_release_death();
}

1
