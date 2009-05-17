package POE::Resource::SIDs;
use Moose::Role;
use strict;

package POE::Kernel;

### Map session IDs to sessions.  Map sessions to session IDs.
### Maintain a sequence number for determining the next session ID.

my %kr_session_ids;
#  ( $session_id => $session_reference,
#    ...,
#  );

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

sub _data_sid_initialize {
  $POE::Kernel::poe_kernel->[POE::Kernel::KR_SESSION_IDS] = \%kr_session_ids;
}

### End-run leak checking.

sub _data_sid_finalize {
  my $finalized_ok = 1;
  while (my ($sid, $ses) = each(%kr_session_ids)) {
    POE::Kernel::_warn "!!! Leaked session ID: $sid = $ses\n";
    $finalized_ok = 0;
  }
  while (my ($ses, $sid) = each(%kr_session_to_id)) {
    POE::Kernel::_warn "!!! Leak sid cross-reference: $ses = $sid\n";
    $finalized_ok = 0;
  }
  return $finalized_ok;
}

### Set a session ID.

sub _data_sid_set {
  my ($self, $sid, $session) = @_;
  $kr_session_ids{$sid} = $session;
  $kr_session_to_id{$session} = $sid;
}

### Clear a session ID.

sub _data_sid_clear {
  my ($self, $session) = @_;
  my $sid = delete $kr_session_to_id{$session};
  if (POE::Kernel::ASSERT_DATA) {
    POE::Kernel::_trap("SID not defined") unless defined $sid;
  }
  delete $kr_session_ids{$sid};
}

### Resolve a session ID into its session.

sub _data_sid_resolve {
  my ($self, $sid) = @_;
  return $kr_session_ids{$sid};
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
