package POE::Kernel;
use File::Spec qw();
use Carp ();

## ORDER SPECIFIC DO NOT FUCK WITH THIS 
BEGIN {
  # Shorthand for defining an assert constant.
	sub _define_constant {
		no strict 'refs';
		no warnings;
		foreach (@_) {
			if ( defined *{$_}{CODE} ) {
				$ENV{"POE_$_"} = &{ *{$_}{CODE} };
				undef *{$_};
			}
		}
	}

	## TRACE_DESTROY not originally in kernel, from session.pm
	## ASSERT_STATES not originally in kernel, from session.pm
  _define_constant(qw(
		TRACE_DEFAULT TRACE_EVENTS TRACE_FILES TRACE_PROFILE TRACE_REFCNT
			TRACE_RETVALS TRACE_SESSIONS TRACE_SIGNALS TRACE_STATISTICS TRACE_DESTROY
		ASSERT_DEFAULT ASSERT_DATA ASSERT_EVENTS ASSERT_FILES ASSERT_RETVALS
			ASSERT_USAGE ASSERT_STATES
		CATCH_EXCEPTIONS
		CHILD_POLLING_INTERVAL
		USE_SIGCHLD
		EVENT_LOOP
		TRACE_FILENAME
	));
}
use POE::Helpers::Constants qw( :all );
use POE::KernelX;
## END ORDER SPECIFIC NONSENSE

our ($poe_kernel, $poe_main_window);

## Called as POE::Kernel->run
sub run {
	my $self = __PACKAGE__->new;
	my $pkx = $self->[-1];
	$pkx->run
}

sub import {
  my ($class, $args) = @_;
  my $package = caller();

	Carp::croak "POE::Kernel expects its arguments in a hash ref"
		if $args && ref($args) ne 'HASH'
	;

	my %validated = exists $args->{loop} ? (loop => delete $args->{loop}) : ();
	Carp::croak "Unknown POE::Kernel import arguments: " . (join ',', keys %$args)
		if %$args
	;

  {
    no strict 'refs';
    *{ $package . '::poe_kernel'      } = \$poe_kernel;
    *{ $package . '::poe_main_window' } = \$poe_main_window;
  }
	
	POE::Kernel->new(\%validated);

}

sub new {
	# TODO - Should KR_ACTIVE_SESSIONS and KR_ACTIVE_EVENT be handled
	# by POE::Resource::Sessions?
	# TODO - Should the subsystems be split off into separate real
	# objects, such as KR_QUEUE is?
	
	unless ( $POE::Kernel::poe_kernel ) {
		my $self = POE::KernelX->instance;
		$POE::Kernel::poe_kernel = bless [
			$self->kr_sessions  # KR_SESSIONS - from POE::Resource::Sessions
			, $self->kr_filenos   # KR_FILENOS - from POE::Resource::FileHandles
			, $self->kr_signals   # KR_SIGNALS - from POE::Resource::Signals
			, $self->kr_aliases   # KR_ALIASES - from POE::Resource::Aliases
			, \$kr_active_session # KR_ACTIVE_SESSION
			, $self->kr_queue     # KR_QUEUE - reference to an object
			, $self->ID           # KR_ID
			, undef               # KR_SESSION_IDS - from POE::Resource::SIDS
			, $self->kr_sid_seq   # KR_SID_SEQ - scalar ref from POE::Resource::SIDS
			, undef               # KR_EXTRA_REFS
			, undef               # KR_SIZE
			, \$kr_run_warning    # KR_RUN
			, \$kr_active_event   # KR_ACTIVE_EVENT
			, $self
		], __PACKAGE__;
	}
	
	$POE::Kernel::poe_kernel;
};

sub AUTOLOAD {
	my ( $self, @args ) = @_;
	our $AUTOLOAD;
	$AUTOLOAD =~ m/.*::(.*?)$/;
	my $method = $1;

	return if $method eq 'DESTROY';
	
	for (@args) {
		$_ = $_->[-1]
			if defined $_ && $_ eq $POE::Kernel::poe_kernel
		;
	}

	## has to cover self being $class_name for ->new and ->run
	## and self being the object POE::Kernel
	return defined $self && ref $self eq 'POE::Kernel' 
		? ( $self->[-1] )->can($method)->($self->[-1], @args)
		: POE::KernelX->instance->can($method)->(@args)
	;
	return $self->can($method)->( @args );
}

1;
