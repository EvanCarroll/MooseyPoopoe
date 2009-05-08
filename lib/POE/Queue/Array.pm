package POE::Queue::Array;
use strict;
use warnings;

use Moose;
use MooseX::AttributeHelpers;
use Errno qw(ESRCH EPERM);
use List::Util ();
use Scalar::Util ();

with 'POE::Queue';

use constant {
	ITEM_PRIORITY => 0
	, ITEM_ID       => 1
	, ITEM_PAYLOAD  => 2
};

use namespace::clean -except => 'meta';

sub import {
	my $package = caller();
	no strict 'refs';
	*{ $package . '::ITEM_PRIORITY' } = \&ITEM_PRIORITY;
	*{ $package . '::ITEM_ID'       } = \&ITEM_ID;
	*{ $package . '::ITEM_PAYLOAD'  } = \&ITEM_PAYLOAD;
}

my %db_id;
my %db_priority;
use MooseX::ClassAttribute;
class_has 'uid' => (
	isa  => 'Int'
	, is => 'ro'
	, default   => 0
	, metaclass => 'Counter'
	, provides  => { inc => 'next_uid' }
);

### Return the next item's priority, undef if the queue is empty.
has '_min_priority' => (
	isa  => 'Maybe[Int]'
	, is => 'ro'
	, lazy    => 1
	, reader => 'get_next_priority'
	, default => sub {
		List::Util::min( keys %db_priority ) || undef
	}
	, clearer => '_reset_min_priority'
);



sub enqueue {
	my ($self, $priority, $payload) = @_;

	# Get the next item ID.  This clever loop will hang indefinitely if
	# you ever run out of integers to store things under.  Map the ID to
	# its due time for search-by-ID functions.

	my $item_id = $self->next_uid;

	my $item_to_enqueue = [
		$priority, # ITEM_PRIORITY
		$item_id,  # ITEM_ID
		$payload,  # ITEM_PAYLOAD
	];

	## Heap holds only valid ref
	$db_id{$item_id} = $item_to_enqueue;
	
	push @{$db_priority{$priority}}, $item_to_enqueue;
	Scalar::Util::weaken( $db_priority{$priority}[-1] );

	return $item_id;
}

### Dequeue the next thing from the queue.  Returns an empty list if
### the queue is empty.  There are different flavors of this
### operation.

sub dequeue_next {
	my $self = shift;
	return unless %db_id && %db_priority;
	
	my $item;
	PRIORITY: while (1) {

		my $priority = $self->get_next_priority;
		return () unless defined $priority;

		while (1) {

			if ( scalar @{ $db_priority{$priority} } ) {
				$item = shift @{ $db_priority{$priority} };
				next unless defined $item;
				last PRIORITY;
			}
			else {
				last;
			}

		}

		## Clear priority table if there are no more items of the same priority
		unless ( @{$db_priority{$priority}} ) {
			delete $db_priority{$priority};
			$self->_reset_min_priority;
		}

	}

	@{ delete $db_id{ $item->[ITEM_ID]} };
}


### Return the number of items currently in the queue.
sub get_item_count { my $self = shift; return scalar keys %db_id }

### Remove an item by its ID.  Takes a coderef filter, too, for
### examining the payload to be sure it really wants to leave.  Sets
### $! and returns undef on failure.
sub remove_item {
	my ($self, $id, $filter) = @_;

	my $item = $db_id{$id};

	my $priority = $item->[ITEM_PRIORITY];

	unless (defined $priority) {
		$! = ESRCH;
		return;
	}

	# Test the item against the filter.
	unless ( $filter->($item->[ITEM_PAYLOAD]) ) {
		$! = EPERM;
		return;
	}

	## Clear priority table if there are no more items of the same priority
	if ( @{ $db_priority{$item->[ITEM_PRIORITY]} } == 1 ) {
		delete $db_priority{ $item->[ITEM_PRIORITY] };
		$self->_reset_min_priority;
	}

	@{  delete $db_id{ $item->[ITEM_ID] }  };

}

### Remove items matching a filter.  Regrettably, this must scan the
### entire queue.  An optional count limits the number of items to
### remove, and it may shorten execution times.  Returns a list of
### references to priority/id/payload lists.  This is intended to
### return all the items matching the filter, and the function's
### behavior is undefined when $count is less than the number of
### matching items.

sub remove_items {
	my $self = shift;
	## Handles filter application
	my @items = $self->peek_items(@_);

	foreach my $item ( @items ) {

		delete $db_id{ $item->[ITEM_ID] };
		
		unless ( @{ $db_priority{$item->[ITEM_PRIORITY]} } ) {
			delete $db_priority{ $item->[ITEM_PRIORITY] };
			$self->_reset_min_priority;
		}

	}
	
	return @items;
}

### Adjust the priority of an item by a relative amount.  Adds $delta
### to the priority of the $id'd object (if it matches $filter), and
### moves it in the queue.
sub adjust_priority {
	my ($self, $id, $filter, $delta) = @_;
	return unless defined $db_id{$id};
	$self->set_priority(  $id, $filter, ($db_id{$id}->[ITEM_PRIORITY] + $delta)  );
}

### Set the priority to a specific amount.  Replaces the item's
### priority with $new_priority (if it matches $filter), and moves it
### to the new location in the queue.
sub set_priority {
	my ($self, $id, $filter, $new_priority) = @_;

	my $item = $db_id{$id};
	return unless defined $item;

	my $old_priority = $item->[ITEM_PRIORITY];
	
	unless (defined $old_priority) {
		$! = ESRCH;
		return;
	}
	
	# Test the item against the filter.
	unless (  $filter->($item->[ITEM_PAYLOAD])  ) {
		$! = EPERM;
		return;
	}

	# Nothing to do if the delta is zero.
	# TODO Actually we may need to ensure that the item is moved to the
	# end of its current priority bucket, since it should have "moved".
	return $new_priority if $new_priority == $old_priority;

	if ( @{ $db_priority{$item->[ITEM_PRIORITY]} } == 1 ) {
		delete $db_priority{ $item->[ITEM_PRIORITY] };
		$self->_reset_min_priority;
	}

	# set the items priority to the new priority
	$item->[ITEM_PRIORITY] = $new_priority;
	
	# delete the item from old priority queue, by removing it by id
	#delete $db_id{ $item->[ITEM_ID] };
	
	# add the item to new priority queue
	push @{ $db_priority{$item->[ITEM_PRIORITY]} }, $item;
	
	# weaken the new reference
	Scalar::Util::weaken( $db_priority{$item->[ITEM_PRIORITY]}->[-1] );

	$item->[ITEM_PRIORITY];

}

### Sanity-check the results of an item insert.  Verify that it
### belongs where it was put.  Only called during debugging.
sub _dump_splice {
	my ($self, $index) = @_;
	my @return;
	my $at = $self->_get_queue($index)->[ITEM_PRIORITY];
	if ($index > 0) {
		my $before = $self->_get_queue($index-1)->[ITEM_PRIORITY];
		push @return, "before($before)";
		Carp::confess "out of order: $before should be < $at" if $before > $at;
	}
	push @return, "at($at)";
	if ($index < $#$self) {
		my $after = $self->_get_queue($index+1)->[ITEM_PRIORITY];
		push @return, "after($after)";
		my @priorities = map {$_->[ITEM_PRIORITY]} @$self;
	
		Carp::confess "out of order: $at should be < $after (@priorities)"
			if $at >= $after
		;
	}
	return "@return";
}

### Peek at items that match a filter.  Returns a list of payloads
### that match the supplied coderef.
sub peek_items {
	my ($self, $filter, $count) = @_;
	$count ||= scalar keys %db_id;

	my @items;
	foreach my $priority ( sort {$b<=>$a} keys %db_priority ) {
		
		foreach my $item ( reverse @{ $db_priority{$priority} } ) {
			## XXX Not Kosher, we skip through undefined (deleted by id)
			next unless defined $item;
		
			if ( $filter->($item->[ITEM_PAYLOAD]) ) {
				unshift @items, $item;
				last unless --$count;
			}

		}

	}

	return @items;
}

__PACKAGE__->meta->make_immutable;

__END__

=head1 NAME

POE::Queue::Array - a high-performance array-based priority queue

=head1 SYNOPSIS

See L<POE::Queue>.

=head1 DESCRIPTION

This class is an implementation of the abstract POE::Queue interface.
As such, its documentation may be found in L<POE::Queue>.

POE::Queue::Array implements a priority queue using Perl arrays,
splice, and copious application of cleverness.

Despite its name, POE::Queue::Array may be used as a stand-alone
priority queue without the rest of POE.

=head1 SEE ALSO

L<POE>, L<POE::Queue>

=head1 BUGS

None known.

=head1 AUTHORS & COPYRIGHTS

Please see L<POE> for more information about authors, contributors,
and POE's licensing.

=cut

# rocco // vim: ts=2 sw=2 expandtab
# TODO - Edit.
