package POE::Filter::Grep;
use strict;

use Moose;

with qw/
	POE::Filter
	POE::Filter::Roles::CodeGetAndPut
/;

sub get_one {
  my $self = shift;

  # Must be a loop so that the buffer will be altered as items are
  # tested.
  while (@{$self->buffer}) {
    my $next_record = shift @{$self->buffer};
    return [ $next_record ] if (
      grep { $self->Get->($_) } $next_record
    );
  }

  return [ ];
}

sub put {
	my ($self, $data) = @_;
	[ grep { $self->Put->($_) } @$data ];
}

sub BUILDARGS {
	my $type = shift;
	Carp::croak "$type must be given an even number of parameters" if @_ & 1;
	my %params = @_;

	Carp::croak "$type requires a Code or both Get and Put parameters"
		unless defined($params{Code})
		or defined($params{Get}) && defined($params{Put})
	;

	\%params;
}

1;

__END__

=head1 NAME

POE::Filter::Grep - select or remove items based on simple rules

=head1 SYNOPSIS

  #!perl

  use POE qw(
    Wheel::FollowTail
    Filter::Line Filter::Grep Filter::Stackable
  );

  POE::Session->create(
    inline_states => {
      _start => sub {
        my $parse_input_as_lines = POE::Filter::Line->new();

        my $select_sudo_log_lines = POE::Filter::Grep->new(
          Put => sub { 1 },
          Get => sub {
            my $input = shift;
            return $input =~ /sudo\[\d+\]/i;
          },
        );

        my $filter_stack = POE::Filter::Stackable->new(
          Filters => [
            $parse_input_as_lines, # first on get, last on put
            $select_sudo_log_lines, # first on put, last on get
          ]
        );

        $_[HEAP]{tailor} = POE::Wheel::FollowTail->new(
          Filename => "/var/log/system.log",
          InputEvent => "got_log_line",
          Filter => $filter_stack,
        );
      },
      got_log_line => sub {
        print "Log: $_[ARG0]\n";
      }
    }
  );

  POE::Kernel->run();
  exit;

=head1 DESCRIPTION

POE::Filter::Grep selects or removes items based on simple tests.  It
may be used to filter input, output, or both.  This filter is named
and modeled after Perl's built-in grep() function.

POE::Filter::Grep is designed to be combined with other filters
through POE::Filter::Stackable.  In the L</SYNOPSIS> example, a filter
stack is created to parse logs as lines and remove all entries that
don't pertain to a sudo process.  (Or if your glass is half full, the
stack only selects entries that DO mention sudo.)

=head1 PUBLIC FILTER METHODS

In addition to the usual POE::Filter methods, POE::Filter::Grep also
supports the following.

=head2 new

new() constructs a new POE::Filter::Grep object.  It must either be
called with a single Code parameter, or both a Put and a Get
parameter.  The values for Code, Put, and Get are code references
that, when invoked, return true to select an item or false to reject
it.  A Code function will be used for both input and output, while Get
and Put functions allow input and output to be filtered in different
ways.  The item in question will be passed as the function's sole
parameter.

  sub reject_bidoofs {
    my $pokemon = shift;
    return 1 if $pokemon ne "bidoof";
    return;
  }

  my $gotta_catch_nearly_all = POE::Filter::Grep->new(
    Code => \&reject_bidoofs,
  );

Enforce read-only behavior:

  my $read_only = POE::Filter::Grep->new(
    Get => sub { 1 },
    Put => sub { 0 },
  );

=head2 modify

modify() changes a POE::Filter::Grep object's behavior at runtime.  It
accepts the same parameters as new(), and it replaces the existing
tests with new ones.

  # Don't give away our Dialgas.
  $gotta_catch_nearly_all->modify(
    Get => sub { 1 },
    Put => sub { return shift() ne "dialga" },
  );

=head1 SEE ALSO

L<POE::Filter> for more information about filters in general.

L<POE::Filter::Stackable> for more details on stacking filters.

=head1 BUGS

None known.

=head1 AUTHORS & COPYRIGHTS

The Grep filter was contributed by Dieter Pearcey.  Documentation is
provided by Rocco Caputo.

Please see the L<POE> manpage for more information about authors and
contributors.

=cut

# rocco // vim: ts=2 sw=2 expandtab
# TODO - Edit.
