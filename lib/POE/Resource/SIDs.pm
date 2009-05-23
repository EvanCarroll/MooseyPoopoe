package POE::Resource::SIDs;
use Moose::Role;
use strict;

use POE::Helpers::Error qw( _warn );
use POE::Helpers::Constants qw( ASSERT_DATA );

### Map session IDs to sessions.  Map sessions to session IDs.
### Maintain a sequence number for determining the next session ID.
#  ( $session_id => $session_reference, )
has 'kr_session_ids' => (
	isa => 'HashRef'
	, is => 'ro'
	, default => sub { +{} }
);

my %kr_session_to_id;
#  ( $session_ref => $session_id,
#    ...,
#  );

##  has [qw/kr_session_ids kr_session_to_id/] => (
##  	isa => 'HashRef'
##  	, is => 'ro'
##  	, default => sub { +{} }
##  )

has 'kr_sid_seq' => (
	isa => 'Int'
	, is => 'rw'
	, default => 1
	, metaclass => 'Counter'
	, provides => {
		inc => '_data_sid_allocate'
	}
);

### End-run leak checking.
sub _data_sid_finalize {
	my $self = shift;
	my $kr_session_ids = $self->kr_session_ids;
  my $finalized_ok = 1;
  while (my ($sid, $ses) = each(%$kr_session_ids)) {
    _warn "!!! Leaked session ID: $sid = $ses\n";
    $finalized_ok = 0;
  }
  while (my ($ses, $sid) = each(%kr_session_to_id)) {
    _warn "!!! Leak sid cross-reference: $ses = $sid\n";
    $finalized_ok = 0;
  }
  return $finalized_ok;
}

### Set a session ID.
sub _data_sid_set {
  my ($self, $sid, $session) = @_;
  $self->kr_session_ids->{$sid} = $session;
  $kr_session_to_id{$session} = $sid;
}

### Clear a session ID.
sub _data_sid_clear {
  my ($self, $session) = @_;
  my $sid = delete $kr_session_to_id{$session};
  if (ASSERT_DATA) {
    POE::Kernel::_trap("SID not defined") unless defined $sid;
  }
  delete $self->kr_session_ids->{$sid};
}

### Resolve a session ID into its session.
sub _data_sid_resolve {
  my ($self, $sid) = @_;
  return $self->kr_session_ids->{$sid};
}

1;

__END__

=head1 NAME

POE::Resource::SIDs - internal session ID manager for POE::Kernel

=head1 SYNOPSIS

There is no public API.

=head1 DESCRIPTION

POE::Resource::SIDs is a mix-in class for POE::Kernel.  It provides
the features necessary to manage session IDs.  It is used internally
by POE::Kernel, so it has no public interface.

=head1 SEE ALSO

See L<POE::Kernel/Session Identifiers (IDs and Aliases)> for more
information about session IDs.

See L<POE::Kernel/Resources> for for public information about POE
resources.

See L<POE::Resource> for general discussion about resources and the
classes that manage them.

=head1 BUGS

None known.

=head1 AUTHORS & COPYRIGHTS

Please see L<POE> for more information about authors and contributors.

=cut

# rocco // vim: ts=2 sw=2 expandtab
# TODO - Edit.
