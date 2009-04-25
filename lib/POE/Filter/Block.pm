package POE::Filter::Block;
use Moose;
use MooseX::AttributeHelpers;
use Moose::Util::TypeConstraints;

use strict;
with 'POE::Filter';

our $VERSION = do {my($r)=(q$Revision: 2447 $=~/(\d+)/);sprintf"1.%04d",$r};

# get() is inherited from POE::Filter.
#------------------------------------------------------------------------------
# 2001-07-27 RCC: The get_one() variant of get() allows Wheel::Xyz to
# retrieve one filtered block at a time.  This is necessary for filter
# changing and proper input flow control.

use namespace::clean -except => 'meta';

subtype 'Natural'
	=> as 'Int'
	=> where { int($_) > 0 }
;
subtype 'LengthCodec'
	=> as 'ArrayRef[CodeRef]'
	=> where { scalar @$_ == 2 }
;

has 'length_codec' => (
	isa         => 'LengthCodec'
	, is        => 'ro'
	, init_arg  => 'LengthCodec'
	, predicate => 'has_length_codec'
);

has 'encoder' => (
	isa  => 'CodeRef'
	, is => 'ro'
	, default => sub {
		my $self = shift;
		
		return $self->has_length_codec
			? $self->length_codec->[0]
			: sub { substr(${$_[0]}, 0, 0) = length(${$_[0]}) . "\0" }
		;
		
	}
);

has 'decoder' => (
	isa => 'CodeRef'
	, is => 'ro'
	, default => sub {
		my $self = shift;

		return $self->has_length_codec
			? $self->length_codec->[1]
			: sub {
				##	unless (${$_[0]} =~ s/^(\d+)\0//s) {
				##		warn length($1), " strange bytes ($1) removed from stream"
				##			if ${$_[0]} =~ s/^(\D+)//s;
				##		return;
				##	}
				##	return $1;
my $stuff = shift;
unless ($$stuff =~ s/^(\d+)\0//s) {
  warn length($1), " strange bytes removed from stream"
    if $$stuff =~ s/^(\D+)//s;
  return;
}
return $1;
				}
		;
	}
);

has 'block_size' => (
	isa         => 'Natural'
	, is        => 'ro'
	, init_arg  => 'BlockSize'
	, predicate => 'has_block_size'
);

has 'expected_size' => (
	isa  => 'Int'
	, is => 'rw'
	, predicate => 'has_expected_size'
	, clearer   => 'clear_expected_size'
);

sub _trigger_blocksize_lengthcodec {
	my $self = shift;
	Carp::croak "Can't use both LengthCodec and BlockSize at the same time"
		if $self->has_block_size && $self->has_length_codec
	;
};

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

sub get_one {
	my $self = shift;

	# Need to check lengths in octets, not characters.
	BEGIN { eval { require bytes } and bytes->import; }

	# If a block size is specified, then pull off a block of that many
	# bytes.
	if ($self->has_block_size) {
		return [ ] unless length($self->buffer) >= $self->block_size;

		my $block = $self->substr_buffer( 0, $self->block_size, '' );
		return [ $block ];
	}

	# Otherwise we're doing the variable-length block thing.  Look for a
	# length marker, and then pull off a chunk of that length.  Repeat.
	else {

		## this just gets the expected size of the block
		## we don't really do much with it though...
		my $expected_size;
		if ( $self->has_expected_size ) {
			$expected_size = $self->expected_size
		}
		else {
			# XXX There should be a way with moose to get the ref straight to the buffer
			#     That way we don't have to copy
			my $buffer = $self->buffer;
			$expected_size = $self->decoder->( \$buffer );
			$self->buffer( $buffer );
		}

		if ( $expected_size ) {
			if ( length($self->buffer) < $expected_size ) {
				$self->expected_size( $expected_size )
			}
			else {
				$self->clear_expected_size;
				return [ $self->substr_buffer( 0, $expected_size, '' ) ]
			}
		}

	}

	return [ ];
}

sub put {
	my ($self, $blocks) = @_;
	my @raw;

	# Need to check lengths in octets, not characters.
	BEGIN { eval { require bytes } and bytes->import; }

	# If a block size is specified, then just assume the put is right.
	# This will cause quiet framing errors on the receiving side.  Then
	# again, we'll have quiet errors if the block sizes on both ends
	# differ.  Ah, well!
	if ($self->has_block_size) {
		@raw = join '', @$blocks;
	}
	# No specified block size. Do the variable-length block thing. This
	# steals a lot of Artur's code from the Reference filter.
	else {
		@raw = @$blocks;
		foreach (@raw) {
			$self->encoder->(\$_);
		}
	}

	\@raw;
}

sub get_pending {
	my $self = shift;
	
	return defined $self->buffer && length $self->buffer
		? [ $self->buffer ]
		: undef
	;
}

sub BUILDARGS {
	my $type = shift;
	Carp::croak "$type : Must be given an even number of parameters" if @_ & 1;
	my %params = @_;
	\%params;
}

__PACKAGE__->meta->make_immutable;


1;

__END__

=head1 NAME

POE::Filter::Block - translate data between streams and blocks

=head1 SYNOPSIS

  #!perl

  use warnings;
  use strict;
  use POE::Filter::Block;

  my $filter = POE::Filter::Block->new( BlockSize => 8 );

  # Prints three lines: abcdefgh, ijklmnop, qrstuvwx.
  # Bytes "y" and "z" remain in the buffer and await completion of the
  # next 8-byte block.

  $filter->get_one_start([ "abcdefghijklmnopqrstuvwxyz" ]);
  while (1) {
    my $block = $filter->get_one();
    last unless @$block;
    print $block->[0], "\n";
  }

  # Print one line: yz123456

  $filter->get_one_start([ "123456" ]);
  while (1) {
    my $block = $filter->get_one();
    last unless @$block;
    print $block->[0], "\n";
  }

=head1 DESCRIPTION

POE::Filter::Block translates data between serial streams and blocks.
It can handle fixed-length and length-prepended blocks, and it may be
extended to handle other block types.

Fixed-length blocks are used when Block's constructor is called with a
BlockSize value.  Otherwise the Block filter uses length-prepended
blocks.

Users who specify block sizes less than one deserve what they get.

In variable-length mode, a LengthCodec parameter may be specified.
The LengthCodec value should be a reference to a list of two
functions: the length encoder, and the length decoder:

  LengthCodec => [ \&encoder, \&decoder ]

The encoder takes a reference to a buffer and prepends the buffer's
length to it.  The default encoder prepends the ASCII representation
of the buffer's length and a chr(0) byte to separate the length from
the actual data:

  sub _default_encoder {
    my $stuff = shift;
    substr($$stuff, 0, 0) = length($$stuff) . "\0";
    return;
  }

The corresponding decoder returns the block length after removing it
and the separator from the buffer.  It returns nothing if no length
can be determined.

  sub _default_decoder {
    my $stuff = shift;
    unless ($$stuff =~ s/^(\d+)\0//s) {
      warn length($1), " strange bytes removed from stream"
        if $$stuff =~ s/^(\D+)//s;
      return;
    }
    return $1;
  }

This filter holds onto incomplete blocks until they are completed.

=head1 PUBLIC FILTER METHODS

POE::Filter::Block has no additional public methods.

=head1 SEE ALSO

Please see L<POE::Filter> for documentation regarding the base
interface.

The SEE ALSO section in L<POE> contains a table of contents covering
the entire POE distribution.

=head1 BUGS

The put() method doesn't verify block sizes.

=head1 AUTHORS & COPYRIGHTS

The Block filter was contributed by Dieter Pearcey, with changes by
Rocco Caputo.

Please see L<POE> for more information about authors and contributors.

=cut

# rocco // vim: ts=2 sw=2 expandtab
# TODO - Edit.
