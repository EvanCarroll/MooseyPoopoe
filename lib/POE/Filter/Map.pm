package POE::Filter::Map;
use strict;

use Moose;

with qw/
	POE::Filter
	POE::Filter::Roles::CodeGetAndPut
	POE::Filter::Roles::ArrayBuffer
/;

use namespace::clean -except => 'meta';

sub get_one {
	my $self = shift;

	return [ ] unless @{$self->buffer};
	my $next_record = shift @{$self->buffer};
	return [ map { $self->Get->($_) } $next_record ];
}

sub put {
	my ($self, $data) = @_;
	[ map { $self->Put->($_) } @$data ];
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

__PACKAGE__->meta->make_immutable;

__END__

=head1 NAME

POE::Filter::Map - transform input and/or output within a filter stack

=head1 SYNOPSIS

  #!perl

  use POE qw(
    Wheel::FollowTail
    Filter::Line Filter::Map Filter::Stackable
  );

  POE::Session->create(
    inline_states => {
      _start => sub {
        my $parse_input_as_lines = POE::Filter::Line->new();

        my $redact_some_lines = POE::Filter::Map->new(
          Code => sub {
            my $input = shift;
            $input = "[REDACTED]" unless $input =~ /sudo\[\d+\]/i;
            return $input;
          },
        );

        my $filter_stack = POE::Filter::Stackable->new(
          Filters => [
            $parse_input_as_lines, # first on get, last on put
            $redact_some_lines, # first on put, last on get
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

POE::Filter::Map transforms data inside the filter stack.  It may be
used to transform input, output, or both depending on how it is
constructed.  This filter is named and modeled after Perl's built-in
map() function.

POE::Filter::Map is designed to be combined with other filters through
POE::Filter::Stackable.  In the L</SYNOPSIS> example, a filter stack
is created to parse logs as lines and redact all entries that don't
pertain to a sudo process.

=head1 PUBLIC FILTER METHODS

In addition to the usual POE::Filter methods, POE::Filter::Map also
supports the following.

=head2 new

new() constructs a new POE::Filter::Map object.  It must either be
called with a single Code parameter, or both a Put and a Get
parameter.  The values for Code, Put and Get are code references that,
when invoked, return transformed versions of their sole parameters.  A
Code function will be used for both input and ouput, while Get and Put
functions allow input and output to be filtered in different ways.

  # Decrypt rot13.
  sub decrypt_rot13 {
    my $encrypted = shift;
    $encrypted =~ tr[a-zA-Z][n-za-mN-ZA-M];
    return $encrypted;
  }

  # Encrypt rot13.
  sub encrypt_rot13 {
    my $plaintext = shift;
    $plaintext =~ tr[a-zA-Z][n-za-mN-ZA-M];
    return $plaintext;
  }

  # Decrypt rot13 on input, and encrypt it on output.
  my $rot13_transcrypter = POE::Filter::Map->new(
    Get => \&decrypt_rot13,
    Put => \&encrypt_rot13,
  );

Rot13 is symmetric, so the above example can be simplified to use a
single Code function.

  my $rot13_transcrypter = POE::Filter::Map->new(
    Code => sub {
      local $_ = shift;
      tr[a-zA-Z][n-za-mN-ZA-M];
      return $_;
    }
  );

=head2 modify

modify() changes a POE::Filter::Map object's behavior at runtime.  It
accepts the same parameters as new(), and it replaces the existing
transforms with new ones.

  # Switch to "reverse" encryption for testing.
  $rot13_transcrypter->modify(
    Code => sub { return scalar reverse shift }
  );

=head1 SEE ALSO

L<POE::Filter> for more information about filters in general.

L<POE::Filter::Stackable> for more details on stacking filters.

=head1 BUGS

None known.

=head1 AUTHORS & COPYRIGHTS

The Map filter was contributed by Dieter Pearcey.  Documentation is
provided by Rocco Caputo.

Please see the L<POE> manpage for more information about authors and
contributors.

=cut

# rocco // vim: ts=2 sw=2 expandtab
# TODO - Edit.
