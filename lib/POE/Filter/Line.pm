package POE::Filter::Line;
use strict;

use Moose;
use POE::Filter::Types::MakeRegexpRef ':all';
use MooseX::ClassAttribute;
use MooseX::Types::Common::String qw/NonEmptyStr/;

with qw/
	POE::Filter
	POE::Filter::Roles::ScalarRefBuffer
/;

use namespace::clean -except => 'meta';

## XXX the original non-moose versoin of this module used to die
##     on unknown args maybe we need MX::StrictConstructor

class_has '_default_input_line_terminator' => (
	isa  => 'RegexpRef'
	, is => 'ro'
	, default => sub { "\x0D\x0A?|\x0A\x0D?" }
);

class_has '_default_output_line_terminator' => (
	isa  => NonEmptyStr
	, is => 'ro'
	, default => "\x0D\x0A"
);

has 'literal' => (
	isa  => NonEmptyStr
	, is => 'ro'
	, init_arg => 'Literal'
);

foreach ( qw/input output/ ) {
	has ( "${_}_literal" => (
		isa  => 'Str | Undef'
		, is => 'rw'
		, metaclass => 'String'
		, provides => {
				append => "_append_to_${_}_literal"
			}
		, init_arg  => ucfirst($_) . 'Literal'
		, predicate => "has_${_}_literal"
		, traits => [qw(Clone)]
	) );
}

has '+output_literal' => ( default => sub {
	$_[0]->literal || $_[0]->_default_output_line_terminator
} );

has '+input_literal' => ( default => sub {
		$_[0]->literal || $_[0]->_default_input_line_terminator
} );


has 'input_regex' => (
	isa        => MakeRegexpRef
	, is       => 'rw'
	, coerce   => 1
	, init_arg => 'InputRegexp'
	, lazy     => 1
	, default  => sub {
		my $self = shift;
		$self->input_literal;
	}
);

has 'autodetect' => (
	isa       => 'Bool'
	, is      => 'ro'
	, lazy    => 1
	, default => sub { $_[0]->input_literal ? 0 : 1 }
);

has '_autodetect_stage' => (
	isa  => 'Int'
	, is => 'rw'
	, default   => 1
	, clearer   => '_autodetect_make_complete'
	, predicate => '_autodetect_in_progress'
	
	, metaclass => 'Counter'
	, provides  => {
		inc => '_autodetect_next_stage'
	}
);


sub get_one {
	my $self = shift;

	# Process as many newlines an we can find.
	LINE: while (1) {

		# Autodetect is done, or it never started.  Parse some buffer!
		if ( ! $self->autodetect || ! $self->_autodetect_in_progress ) {
			#DEBUG and warn unpack 'H*', $self->[INPUT_REGEXP];

			my $re = $self->input_regex;

			last LINE unless ${$self->buffer} =~ s/^(.*?)($re)//s;
			#DEBUG and warn "got line: <<", unpack('H*', $1), ">>\n";

			return [ $1 ];
		}
		else {

			# Waiting for the first line ending.  Look for a generic newline.
			if ( $self->_autodetect_stage == 1 ) {
				my $line_term = $self->_default_input_line_terminator;
				last LINE unless ${$self->buffer} =~ s/^(.*?)($line_term)//;

				my $line = $1;

				# The newline can be complete under two conditions.  First: If
				# it's two characters.  Second: If there's more data in the
				# framing buffer.  Loop around in case there are more lines.
				if ( length($2) == 2 or length ${$self->buffer} ) {
					#DEBUG and warn "detected complete newline after line: <<$1>>\n";
					$self->input_literal( $2 );
					$self->_autodetect_make_complete
				}

				# The regexp has matched a potential partial newline.  Save it,
				# and move to the next state.  There is no more data in the
				# framing buffer, so we're done.
				else {
					#DEBUG and warn "detected suspicious newline after line: <<$1>>\n";
					$self->input_literal( $2 );
					$self->_autodetect_next_stage
				}

				return [ $line ];
			}

			# Waiting for the second line beginning.  Bail out if we don't
			# have anything in the framing buffer.
			if ( $self->_autodetect_stage == 2 ) {
				return [ ] unless length ${$self->buffer};

				#DEBUG and warn "completed newline after line: <<$1>>\n";
				$self->_append_to_input_literal( substr(${$self->buffer}, 0, 1, '') )
					if substr(${$self->buffer},0, 1) eq ( $self->input_literal eq "\x0D" ? "\x0A" : "\x0D" )
				;

				# Regardless, whatever is in INPUT_REGEXP is now a complete
				# newline.  End autodetection, post-process the found newline,
				# and loop to see if there are other lines in the buffer.
				$self->_autodetect_make_complete;
			}

		}

	}

	return [ ];
}

#------------------------------------------------------------------------------
# New behavior.  First translate system newlines ("\n") into whichever
# newlines are supposed to be sent.  Second, add a trailing newline if
# one doesn't already exist.  Since the referenced output list is
# supposed to contain one line per element, we also do a split and
# join.  Bleah. ... why isn't the code doing what the comment says?

sub put {
	my ($self, $lines) = @_;
	[map $_ . $self->output_literal, @$lines]
}

## Here there be dragons - this stupid fucking framework
## thought it was good idea to marshell a flag as an undef state
## for a totally seperate paramater (autodetect on undef)
sub BUILDARGS {
	my $type = shift;
	Carp::croak "$type requires an even number of parameters" if @_ and @_ & 1;
	my %params = @_;

	if ( exists $params{Literal} ) {
		Carp::croak 'Conflicting arguments: Literal and InputLiteral, OutputLiteral, autodetect, or InputRegex'
			if exists $params{OutputLiteral}
			|| exists $params{InputLiteral}
			|| exists $params{autodetect}
			|| exists $params{InputRegexp}
		;
		if ( !defined $params{Literal} ) { ## Literal Expansion
			delete $params{Literal};
			$params{autodetect} ||= 1;
		}
	}
	
	if ( exists $params{InputLiteral} ) {
		if ( exists $params{InputRegexp} ) {
			Carp::croak "Conflicting arguments: cannot have both InputRegexp and InputLiteral (or Literal)"
		}
		elsif ( !defined $params{InputLiteral} ) {
			delete $params{InputLiteral};
			$params{autodetect} ||= 1;
		}
	}
	
	\%params
}


__PACKAGE__->meta->make_immutable;

__END__

=head1 NAME

POE::Filter::Line - serialize and parse terminated records (lines)

=head1 SYNOPSIS

  #!perl

  use POE qw(Wheel::FollowTail Filter::Line);

  POE::Session->create(
    inline_states => {
      _start => sub {
        $_[HEAP]{tailor} = POE::Wheel::FollowTail->new(
          Filename => "/var/log/system.log",
          InputEvent => "got_log_line",
          Filter => POE::Filter::Line->new(),
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

POE::Filter::Line parses stream data into terminated records.  The
default parser interprets newlines as the record terminator, and the
default serializer appends network newlines (CR/LF, or "\x0D\x0A") to
outbound records.

Record terminators are removed from the data POE::Filter::Line
returns.

POE::Filter::Line supports a number of other ways to parse lines.
Constructor parameters may specify literal newlines, regular
expressions, or that the filter should detect newlines on its own.

=head1 PUBLIC FILTER METHODS

POE::Filter::Line's new() method has some interesting parameters.

=head2 new

new() accepts a list of named parameters.

In all cases, the data interpreted as the record terminator is
stripped from the data POE::Filter::Line returns.

C<InputLiteral> may be used to parse records that are terminated by
some literal string.  For example, POE::Filter::Line may be used to
parse and emit C-style lines, which are terminated with an ASCII NUL:

  my $c_line_filter = POE::Filter::Line->new(
    InputLiteral => chr(0),
    OutputLiteral => chr(0),
  );

C<OutputLiteral> allows a filter to put() records with a different
record terminator than it parses.  This can be useful in applications
that must translate record terminators.

C<Literal> is a shorthand for the common case where the input and
output literals are identical.  The previous example may be written
as:

  my $c_line_filter = POE::Filter::Line->new(
    Literal => chr(0),
  );

An application can also allow POE::Filter::Like to figure out which
newline to use.  This is done by specifying C<InputLiteral> to be
undef:

  my $whichever_line_filter = POE::Filter::Line->new(
    InputLiteral => undef,
    OutputLiteral => "\n",
  );

C<InputRegexp> may be used in place of C<InputLiteral> to recognize
line terminators based on a regular expression.  In this example,
input is terminated by two or more consecutive newlines.  On output,
the paragraph separator is "---" on a line by itself.

  my $paragraph_filter = POE::Filter::Line->new(
    InputRegexp => "([\x0D\x0A]{2,})",
    OutputLiteral => "\n---\n",
  );

=head1 PUBLIC FILTER METHODS

POE::Filter::Line has no additional public methods.

=head1 SEE ALSO

Please see L<POE::Filter> for documentation regarding the base
interface.

The SEE ALSO section in L<POE> contains a table of contents covering
the entire POE distribution.

=head1 BUGS

The default input newline parser is a regexp that has an unfortunate
race condition.  First the regular expression:

  /(\x0D\x0A?|\x0A\x0D?)/

While it quickly recognizes most forms of newline, it can sometimes
detect an extra blank line.  This happens when a two-byte newline
character is broken between two reads.  Consider this situation:

  some stream dataCR
  LFother stream data

The regular expression will see the first CR without its corresponding
LF.  The filter will properly return "some stream data" as a line.
When the next packet arrives, the leading "LF" will be treated as the
terminator for a 0-byte line.  The filter will faithfully return this
empty line.

B<It is advised to specify literal newlines or use the autodetect
feature in applications where blank lines are significant.>

=head1 AUTHORS & COPYRIGHTS

Please see L<POE> for more information about authors and contributors.

=cut

# rocco // vim: ts=2 sw=2 expandtab
# TODO - Edit.
