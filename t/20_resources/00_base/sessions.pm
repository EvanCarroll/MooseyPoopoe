# $Id: sessions.pm 1971 2006-05-30 20:32:30Z bsmith $

use strict;

use lib qw(./mylib ../mylib);
use Test::More tests => 59;

sub POE::Kernel::ASSERT_DEFAULT () { 1 }
sub POE::Kernel::TRACE_DEFAULT  () { 1 }
sub POE::Kernel::TRACE_FILENAME () { "./test-output.err" }

BEGIN { use_ok("POE") }

# POE::Kernel is used as a parent session.  Gather a baseline
# reference count for it.  Its value will be used for other tests.

my $base_kernel_refcount = $poe_kernel->_data_ses_refcount($poe_kernel);

ok($poe_kernel->_data_ses_count() == 1, "only POE::Kernel exists");

# Allocate a dummy session for testing.

my $child     = bless [ ], "POE::Session";
my $child_sid = $poe_kernel->_data_sid_allocate();

$poe_kernel->_data_ses_allocate(
  $child,      # session
  $child_sid,  # sid
  $poe_kernel, # parent
);

my $base_child_refcount = $poe_kernel->_data_ses_refcount($child);

# Play a brief game with reference counts.  Make sure negative ones
# cause errors.

eval { $poe_kernel->_data_ses_refcount_dec($child) };
ok(
  $@ && $@ =~ /reference count went below zero/,
  "trap on negative reference count"
);

eval { $poe_kernel->_data_ses_collect_garbage($child) };
ok(
  $@ && $@ =~ /has a reference count inconsistency/,
  "refcount inconsistency during garbage collection"
);

$poe_kernel->_data_ses_refcount_inc($child);
ok(
  $poe_kernel->_data_ses_refcount($child) == $base_child_refcount,
  "incremented reference count is back to base"
);

# Ensure that the session's ID was set.

ok(
  $poe_kernel->_data_sid_resolve($child_sid) == $child,
  "child session's ID is correct"
);

# Ensure parent/child referential integrity.

{ my @children = $poe_kernel->_data_ses_get_children($poe_kernel);
  ok(
    eq_array(\@children, [ $child ]),
    "POE::Kernel has only the child session"
  );

  ok(
    $poe_kernel->_data_ses_refcount($poe_kernel) == $base_kernel_refcount + 1,
    "POE::Kernel's refcount incremented by child"
  );

  my $parent = $poe_kernel->_data_ses_get_parent($child);
  ok($parent == $poe_kernel, "child's parent is POE::Kernel");

  ok(
    $poe_kernel->_data_ses_is_child($poe_kernel, $child),
    "child is child of POE::Kernel"
  );

  ok($poe_kernel->_data_ses_count() == 2, "two sessions now");
}

# Try to free POE::Kernel while it has a child session.

eval { $poe_kernel->_data_ses_free($poe_kernel) };
ok(
  $@ && $@ =~ /no parent to give children to/,
  "can't free POE::Kernel while it has children"
);

# A variety of session resolution tests.

ok(
  $poe_kernel->_data_ses_resolve("$child") == $child,
  "stringified reference resolves to blessed one"
);

ok(
  !defined($poe_kernel->_data_ses_resolve("nonexistent")),
  "nonexistent stringy reference doesn't resolve"
);

ok(
  $poe_kernel->_data_ses_resolve_to_id($child) == $child_sid,
  "session reference resolves to ID"
);

ok(
  !defined($poe_kernel->_data_ses_resolve_to_id("nonexistent")),
  "nonexistent session reference doesn't resolve"
);

# Create a grandchild session (child of child).  Verify that its place
# in the grand scheme of things is secure.

my $grand    = bless [ ], "POE::Session";
my $grand_id = $poe_kernel->_data_sid_allocate();

$poe_kernel->_data_ses_allocate(
  $grand,      # session
  $grand_id,   # sid
  $child,      # parent
);

my $base_grand_refcount = $poe_kernel->_data_ses_refcount($grand);

{ my @children = $poe_kernel->_data_ses_get_children($child);
  ok(
    eq_array(\@children, [ $grand ]),
    "child has only the grandchild session"
  );

  ok(
    $poe_kernel->_data_ses_refcount($child) == $base_child_refcount + 1,
    "child refcount incremented by the grandchild"
  );

  my $parent = $poe_kernel->_data_ses_get_parent($grand);
  ok($parent == $child, "grandchild's parent is child");

  ok(
    $poe_kernel->_data_ses_is_child($child, $grand),
    "grandchild is child of child"
  );

  ok($poe_kernel->_data_ses_count() == 3, "three sessions now");
}

{ my @children = $poe_kernel->_data_ses_get_children($poe_kernel);
  ok(
    eq_array(\@children, [ $child ]),
    "POE::Kernel children untouched by grandchild"
  );

  ok(
    $poe_kernel->_data_ses_refcount($poe_kernel) == $base_kernel_refcount + 1,
    "POE::Kernel's refcount untouched by grandchild"
  );
}

# Create a great-grandchild session (child of grandchild).  Verify
# that its place in the grand scheme of things is secure.

my $great    = bless [ ], "POE::Session";
my $great_id = $poe_kernel->_data_sid_allocate();

$poe_kernel->_data_ses_allocate(
  $great,      # session
  $great_id,   # sid
  $grand,      # parent
);

my $base_great_refcount = $poe_kernel->_data_ses_refcount($great);

{ my @children = $poe_kernel->_data_ses_get_children($grand);
  ok(
    eq_array(\@children, [ $great ]),
    "grandchild has only the great-grandchild session"
  );

  ok(
    $poe_kernel->_data_ses_refcount($grand) == $base_grand_refcount + 1,
    "grandchild refcount incremented by the great-grandchild"
  );

  my $parent = $poe_kernel->_data_ses_get_parent($great);
  ok($parent == $grand, "great-grandchild's parent is grandchild");

  ok(
    $poe_kernel->_data_ses_is_child($child, $grand),
    "great-grandchild is child of grandchild"
  );
}

{ my @children = $poe_kernel->_data_ses_get_children($poe_kernel);
  ok(
    eq_array(\@children, [ $child ]),
    "POE::Kernel children untouched by great-grandchild"
  );

  ok(
    $poe_kernel->_data_ses_refcount($poe_kernel) == $base_kernel_refcount + 1,
    "POE::Kernel's refcount untouched by great-grandchild"
  );
}

{ my @children = $poe_kernel->_data_ses_get_children($child);
  ok(
    eq_array(\@children, [ $grand ]),
    "child children untouched by great-grandchild"
  );

  ok(
    $poe_kernel->_data_ses_refcount($child) == $base_child_refcount + 1,
    "child's refcount untouched by great-grandchild"
  );
}

{ my @children = $poe_kernel->_data_ses_get_children($great);
  ok(@children == 0, "no great-great-grandchildren");
}

# Move the grandchild to just under POE::Kernel.  This makes child and
# grandchild siblings.

$poe_kernel->_data_ses_move_child($grand, $poe_kernel);

ok(
  $poe_kernel->_data_ses_get_parent($child) == $poe_kernel,
  "child's parent is POE::Kernel"
);

ok(
  $poe_kernel->_data_ses_get_parent($grand) == $poe_kernel,
  "grandchild's parent is POE::Kernel"
);

{ my @children = $poe_kernel->_data_ses_get_children($poe_kernel);
  my %kids = map {($_=>1)} @children;

  ok(exists($kids{$child}), "POE::Kernel owns child");
  ok(exists $kids{$grand}, "POE::Kernel owns grandchild");

  ok(
    $poe_kernel->_data_ses_refcount($poe_kernel) == $base_kernel_refcount + 2,
    "POE::Kernel refcount increased since inheriting grandchild"
  );
}

{ my @children = $poe_kernel->_data_ses_get_children($child);
  ok( eq_array(\@children, [ ]), "child has no children" );

  ok(
    $poe_kernel->_data_ses_refcount($child) == $base_child_refcount,
    "child's refcount decreased since losing grandchild"
  );
}

# Free the childless child.  Make sure POE::Kernel/child data
# structures cross-reference.

$poe_kernel->_data_ses_free($child);

{ my @children = $poe_kernel->_data_ses_get_children($poe_kernel);
  ok(
    eq_array(\@children, [ $grand ]),
    "POE::Kernel only has grandchild now"
  );

  my $parent = $poe_kernel->_data_ses_get_parent($grand);
  ok($parent == $poe_kernel, "grandchild's parent is POE::Kernel");

  ok(
    $poe_kernel->_data_ses_refcount($poe_kernel) == $base_kernel_refcount + 1,
    "POE::Kernel's refcount decremented on child loss"
  );

  eval { my $parent = $poe_kernel->_data_ses_get_parent($child) };
  ok(
    $@ && $@ =~ /retrieving parent of a nonexistent session/,
    "can't get parent of nonexistent session"
  );

  eval { my $parent = $poe_kernel->_data_ses_get_children($child) };
  ok(
    $@ && $@ =~ /retrieving children of a nonexistent session/,
    "can't get children of nonexistent session"
  );

  eval { my $parent = $poe_kernel->_data_ses_is_child($child, $child) };
  ok(
    $@ && $@ =~ /testing is-child of a nonexistent parent session/,
    "can't test is-child of nonexistent session"
  );
}

# Stop the grandchild.  The great-grandchild will be inherited by
# POE::Kernel after this.

$poe_kernel->_data_ses_stop($grand);

{ my @children = $poe_kernel->_data_ses_get_children($poe_kernel);
  ok(
    eq_array(\@children, [ $great ]),
    "POE::Kernel only has great-grandchild now"
  );

  my $parent = $poe_kernel->_data_ses_get_parent($great);
  ok($parent == $poe_kernel, "great-grandchild's parent is POE::Kernel");

  ok(
    $poe_kernel->_data_ses_refcount($poe_kernel) == $base_kernel_refcount + 1,
    "POE::Kernel's refcount conserved"
  );
}

# Try garbage collection on a session that can use stopping.

$poe_kernel->_data_ses_collect_garbage($great);

{ my @children = $poe_kernel->_data_ses_get_children($poe_kernel);
  ok(
    eq_array(\@children, [ ]),
    "POE::Kernel has no children anymore"
  );

  ok(
    $poe_kernel->_data_ses_refcount($poe_kernel) == $base_kernel_refcount,
    "POE::Kernel's refcount back to basics"
  );
}

# Test traps for dealing with nonexistent sessions.

eval { $poe_kernel->_data_ses_refcount_inc("nonexistent") };
ok (
  $@ && $@ =~ /incrementing refcount for nonexistent session/,
  "can't increment refcount for nonexistent session"
);

eval { $poe_kernel->_data_ses_refcount_dec("nonexistent") };
ok (
  $@ && $@ =~ /decrementing refcount of a nonexistent session/,
  "can't decrement refcount for nonexistent session"
);

eval { $poe_kernel->_data_ses_collect_garbage("nonexistent") };
ok(
  $@ && $@ =~ /collecting garbage for a nonexistent session/,
  "can't collect garbage for nonexistent session"
);

eval { $poe_kernel->_data_ses_stop("nonexistent") };
ok(
  $@ && $@ =~ /stopping a nonexistent session/,
  "can't stop a nonexistent session"
);

# Attempt to allocate a session for a nonexistent parent.

my $bogus     = bless [ ], "POE::Session";
my $bogus_sid = $poe_kernel->_data_sid_allocate();

eval {
  $poe_kernel->_data_ses_allocate(
    $bogus,        # session
    $bogus_sid,    # sid
    "nonexistent", # parent
  )
};
ok(
  $@ && $@ =~ /parent nonexistent does not exist/,
  "can't allocate a session for an unknown parent"
);

# Attempt to allocate a session that already exists.

eval {
  $poe_kernel->_data_ses_allocate(
    $poe_kernel, # session
    $bogus_sid,  # sid
    $poe_kernel, # parent
  )
};
ok(
  $@ && $@ =~ /session .*? is already allocated/,
  "can't allocate a session that's already allocated"
);

# Attempt to move nonexistent sessions around.

eval { $poe_kernel->_data_ses_move_child("nonexistent", $poe_kernel) };
ok(
  $@ && $@ =~ /moving nonexistent child to another parent/,
  "can't move nonexistent child to another parent"
);

eval { $poe_kernel->_data_ses_move_child($poe_kernel, "nonexistent") };
ok(
  $@ && $@ =~ /moving child to a nonexistent parent/,
  "can't move a session to a nonexistent parent"
);

# Free the last session, and finalize the subsystem.  Freeing it is
# necessary because the original refcount includes some events that
# would otherwise count as leakage during finalization.

$poe_kernel->_data_ses_stop($poe_kernel);

ok($poe_kernel->_data_ses_finalize(), "finalized POE::Resource::Sessions");

1;
