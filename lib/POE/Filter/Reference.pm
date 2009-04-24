# $Id: Reference.pm 2447 2009-02-17 05:04:43Z rcaputo $

# Filter::Reference partial copyright 1998 Artur Bergman
# <artur@vogon-solutions.com>.  Partial copyright 1999 Philip Gwyn.

package POE::Filter::Reference;

use Moose;
use POE::Filter;
use Carp qw();

with 'POE::Filter';

our $VERSION = do {my($r)=(q$Revision: 2447 $=~/(\d+)/);sprintf"1.%04d",$r};

use Moose::Util::TypeConstraints;

use namespace::clean -except => 'meta';

## Accepts the name of package that might or might not be loaded
## Or, the name of the package the loaded package that has the methods
## Or, an object.
## Because it excepts a package name you can't use delegation

## XXX this should really be a class_has in my eyes. Davey doesn't want to fix his bugs.
## http://rt.cpan.org/Public/Bug/Display.html?id=45260
has 'freezer' => (
	isa   => 'Object | Str'
	, is  => 'ro'
	## XXX This should really default to simply Storable
	##     default should not be install-independant.
	, default => sub {
  	my @packages = qw(Storable FreezeThaw YAML);
	  foreach my $package (@packages) {
	    eval { Class::MOP::load_class( $package ) };
	    if ($@) {
	      warn $@;
	      next;
	    }
	    return $package; # Found a good freezer!
	  }
		die "Filter::Reference requires one of @packages";
	}
	, initializer => sub {
		my ( $self, $freezer, $sub, $meta ) = @_;
		
		## XXX This is here because the :136 :138 test in 07_reference.t 
		## borks the symbol table with Symbol::delete_package
		## Aparently, POE is supposed to just randomly re-require it
		## Class::MOP::load_class( $freezer ) doesn't work.
		unless (
			ref $freezer
			|| Class::MOP::is_class_loaded( $freezer )
		) {
			local %INC;
			delete $INC{$freezer};
			Class::MOP::load_class($freezer);
		}

		warn "$freezer doesn't have a freeze or nfreeze method\n"
			unless $freezer->can('freeze')
			|| $freezer->can('nfreeze')
		;
	  warn "$freezer doesn't have a thaw method\n"
			unless $freezer->can('thaw')
		;
		
		$sub->( $freezer ) if $sub;
	}
);

## XXX Tests attempt to reload the module, exactly once if these two subs aren't there
##     no fucking idea why -EC
## XXX Original non-poe version also warned twice if freeze and thaw couldn't be set, and then returned
##     undef from new, this is fucking stupid anyway, and there were no tests for it ;)
has 'freeze' => (
	isa        => 'CodeRef'
	, is       => 'ro'
	, init_arg => undef
	, lazy     => 1 ## Required because depends on initializer
	, default  => sub {
		my $self = shift;
		my $sub = $self->freezer->can('nfreeze') || $self->freezer->can('freeze');

		unless ( $sub ) {
			$self->meta->get_attribute('freezer')->initializer->( $self, $self->freezer );
			$sub = $self->freezer->can('nfreeze') || $self->freezer->can('freeze');
		}

		Carp::croak ( "Even after attemping to reload this module we don't have a freeze()/nfreeze() on freezer" )
			unless $sub
		;
		$sub;
	}
);
has 'thaw' => (
	isa        => 'CodeRef'
	, is       => 'ro'
	, init_arg => undef
	, lazy     => 1
	, default  => sub {
		my $self = shift;
		my $sub = $self->freezer->can('thaw');

		unless ( $sub ) {
			$self->meta->get_attribute('freezer')->initializer->( $self, $self->freezer );
			$sub = $self->freezer->can('that');
		}

		Carp::croak ( "Even after attemping to reload this module we don't have a thaw() on freezer" )
			unless $sub
		;
		$sub;
	}
);

has 'buffer' => ( isa => 'Str', is => 'rw' );

## Why couldn't this init_arg just be compression
has 'compress' => (
	isa        => 'Str' ## XXX Tests require accepting int(9) as true -EC
	, is       => 'ro'
	, init_arg => 'compression'
	, default  => 0
	, initializer  => sub {
		my ( $self, $value, $sub, $meta ) = @_;
		if ( Class::MOP::load_class('Compress::Zlib') ) {
			$sub->( $value );
		}
		else {
			Carp::croak "Compress::Zlib load failed with error: $@\n";
		}
	}
);

# 2001-07-27 RCC: The get_one() variant of get() allows Wheel::Xyz to
# retrieve one filtered block at a time.  This is necessary for filter
# changing and proper input flow control.
sub get_one_start {
  my ($self, $stream) = @_;
  $self->{BUFFER} .= join('', @$stream);
}

sub get_one {
  my $self = shift;

  # Need to check lengths in octets, not characters.
  BEGIN { eval { require bytes } and bytes->import; }

  if (
    $self->{BUFFER} =~ /^(\d+)\0/ and
    length($self->{BUFFER}) >= $1 + length($1) + 1
  ) {
    substr($self->{BUFFER}, 0, length($1) + 1) = "";
    my $return = substr($self->{BUFFER}, 0, $1);
    substr($self->{BUFFER}, 0, $1) = "";
    $return = Compress::Zlib::uncompress($return) if $self->compress;
    return [ $self->thaw->($return) ];
  }

  return [ ];
}

# freeze one or more references, and return a string representing them
sub put {
  my ($self, $references) = @_;

  # Need to check lengths in octets, not characters.
  BEGIN { eval { require bytes } and bytes->import; }

	my @raw;
  foreach my $ref ( @$references ) {
		my $frozen = $self->freeze->($ref);
		$frozen = Compress::Zlib::compress($frozen) if $self->compress;
		push @raw, length($frozen) . "\0" . $frozen;
	}

  \@raw;
}

# Return everything we have outstanding.  Do not destroy our framing
# buffer, though.
sub get_pending {
  my $self = shift;
  return undef unless length $self->{BUFFER};
  return [ $self->{BUFFER} ];
}


## Because this shitty framework wants list arguments
sub BUILDARGS {
	my $args;
	
	if ( ref $_[0] eq 'HASH' ) {
		$args = $_[0];
	}
	else {
		$args->{type}        ||= $_[0] if defined $_[0];
		$args->{freezer}     ||= $_[1] if defined $_[1];
		$args->{compression} ||= $_[2] if defined $_[2];
	}

	$args;
}

__PACKAGE__->meta->make_immutable;

__END__

=head1 NAME

POE::Filter::Reference - freeze and thaw arbitrary Perl data

=head1 SYNOPSIS

  #!perl

  use YAML;
  use POE qw(Wheel::ReadWrite Filter::Reference);

  POE::Session->create(
    inline_states => {
      _start => sub {
        pipe(my($read, $write)) or die $!;
        $_[HEAP]{io} = POE::Wheel::ReadWrite->new(
          InputHandle => $read,
          OutputHandle => $write,
          Filter => POE::Filter::Reference->new(),
          InputEvent => "got_perl_data",
        );

        $_[HEAP]{io}->put(
          { key_1 => 111, key_2 => 222 }
        );
      },
      got_perl_data => sub {
        print "Got data:\n", YAML::Dump($_[ARG0]);
        print "Bye!\n";
        delete $_[HEAP]{io};
      }
    }
  );

  POE::Kernel->run();
  exit;

=head1 DESCRIPTION

POE::Filter::Reference allows programs to send and receive arbitrary
Perl data structures without worrying about a line protocol.  Its
put() method serializes Perl data into a byte stream suitable for
transmission.  get_one() parses the data structures back out of such a
stream.

By default, POE::Filter::Reference uses Storable to do its magic.  A
different serializer may be specified at construction time.

=head1 PUBLIC FILTER METHODS

POE::Filter::Reference deviates from the standard POE::Filter API in
the following ways.

=head2 new [SERIALIZER [, COMPRESSION]]

new() creates and initializes a POE::Filter::Reference object.  It
will use Storable as its default SERIALIZER if none other is
specified.

If COMPRESSION is true, Compress::Zlib will be called upon to reduce
the size of serialized data.  It will also decompress the incoming
stream data.

Any class that supports nfreeze() (or freeze()) and thaw() may be used
as a SERIALIZER.  If a SERIALIZER implements both nfreeze() and
freeze(), then the "network" version will be used.

SERIALIZER may be a class name:

  # Use Storable explicitly, specified by package name.
  my $filter = POE::Filter::Reference->new("Storable");

  # Use YAML instead.  Compress its output, as it may be verbose.
  my $filter = POE::Filter::Reference->new("YAML", 1);

SERIALIZER may also be an object:

  # Use an object.
  my $serializer = Data::Serializer::Something->new();
  my $filter = POE::Filter::Reference->new($serializer);

If SERIALIZER is omitted or undef, the Reference filter will try to
use Storable, FreezeThaw, and YAML in that order.
POE::Filter::Reference will die if it cannot find one of these
serializers, but this rarely happens now that Storable and YAML are
bundled with Perl.

  # A choose-your-own-serializer adventure!
  # We'll still deal with compressed data, however.
  my $filter = POE::Filter::Reference->new(undef, 1);

POE::Filter::Reference will try to compress frozen strings and
uncompress them before thawing if COMPRESSION is true.  It uses
Compress::Zlib for this.  POE::Filter::Reference doesn't need
Compress::Zlib if COMPRESSION is false.

new() will try to load any classes it needs.

=head1 SERIALIZER API

Here's what POE::Filter::Reference expects of its serializers.

=head2 thaw SERIALIZED

thaw() is required.  It accepts two parameters: $self and a scalar
containing a SERIALIZED byte stream representing a single Perl data
structure.  It returns a reconstituted Perl data structure.

  sub thaw {
    my ($self, $stream) = @_;
    my $reference = $self->_deserialization_magic($stream);
    return $reference;
  }

=head2 nfreeze REFERENCE

Either nfreeze() or freeze() is required.  They behave identically,
except that nfreeze() is guaranteed to be portable across networks and
between machine architectures.

These freezers accept two parameters: $self and a REFERENCE to Perl
data.  They return a serialized version of the REFERENCEd data.

  sub nfreeze {
    my ($self, $reference) = @_;
    my $stream = $self->_serialization_magic($reference);
    return $stream;
  }

=head2 freeze REFERENCE

freeze() is an alternative form of nfreeze().  It has the same call
signature as nfreeze(), but it doesn't guarantee that serialized data
will be portable across machine architectures.

If you must choose between implementing freeze() and nfreeze() for use
with POE::Filter::Reference, go with nfreeze().

=head1 SEE ALSO

Please see L<POE::Filter> for documentation regarding the base
interface.

The SEE ALSO section in L<POE> contains a table of contents covering
the entire POE distribution.

=head1 BUGS

Not so much bugs as caveats:

It's important to use identical serializers on each end of a
connection.  Even different versions of the same serializer can break
data in transit.

Most (if not all) serializers will rebless data at the destination,
but many of them will not load the necessary classes to make their
blessings work.

=head1 AUTHORS & COPYRIGHTS

The Reference filter was contributed by Artur Bergman, with changes
by Philip Gwyn.

Please see L<POE> for more information about authors and contributors.

=cut

# rocco // vim: ts=2 sw=2 expandtab
# TODO - Edit.
