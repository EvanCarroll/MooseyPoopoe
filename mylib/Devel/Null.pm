# $Id: Null.pm 2357 2008-06-20 17:41:54Z rcaputo $

# This `perl -d` debugging module is an ad-hoc custom debugger.  It's
# optional, and it may not even work.

use strict;

use vars qw($VERSION);
$VERSION = do {my($r)=(q$Revision: 2357 $=~/(\d+)/);sprintf"1.%04d",$r};

package Null; # satisfies 'use'

package DB;
use vars qw($sub);
use Carp;

# This bit traces execution immediately before a given condition.
# It's used to find out where in hell something went wrong.
my @trace = ("no step") x 16;

sub DB {
  my ($package, $file, $line) = caller;

  my $discard = shift @trace;
  push @trace, "step @ $file:$line: ";

  if ( defined($POE::Kernel::poe_kernel)
       and @{$POE::Kernel::poe_kernel->[8]}
       and $POE::Kernel::poe_kernel->[8]->[0]->[2] =~ /\-\</
     ) {
    $| = 1;
    print join("\n", @trace), "\n";
    kill -9, $$;
    exit;
  }

#  print "step @ $file:$line\n";
}

sub sub {
  my ($package, $file, $line) = caller;
#  print "sub $sub @ $file:$line\n";
  no strict 'refs';
  &$sub;
}

1;
