package POE::Helpers::Constants;

use Sub::Exporter -setup => {
	exports => [ qw(
		MODE_RD MODE_WR MODE_EX

		EN_CHILD EN_GC EN_PARENT EN_SCPOLL EN_SIGNAL
		EN_START EN_STAT EN_STOP
	
		CHILD_GAIN CHILD_LOSE CHILD_CREATE

		ET_POST  ET_CALL  ET_START  ET_STOP  ET_SIGNAL
		 ET_GC  ET_PARENT  ET_CHILD  ET_SCPOLL  ET_ALARM
		 ET_SELECT  ET_STAT  ET_SIGCLD  ET_MASK_USER
	) ]

	, groups => {
		filehandle => [qw( MODE_RD MODE_WR MODE_EX )]
		, events   => [qw(
			EN_CHILD EN_GC EN_PARENT EN_SCPOLL EN_SIGNAL
			EN_START EN_STAT EN_STOP
		)]
		, child    => [qw(
			CHILD_GAIN CHILD_LOSE CHILD_CREATE
		)]
	}
};

# Filehandle activity modes.
# They are often used as list indexes.
# Used in Resources/ and Loops/
use constant {
	MODE_RD   => 0 # read
	, MODE_WR => 1 # write
	, MODE_EX => 2 # exception/expedite
};


# These are the names of POE's internal events.
# Used in Resources/Sessions
use constant {
	EN_CHILD    => '_child'
	, EN_GC     => '_garbage_collect'
	, EN_PARENT => '_parent'
	, EN_SCPOLL => '_sigchld_poll'
	, EN_SIGNAL => '_signal'
	, EN_START  => '_start'
	, EN_STAT   => '_stat_tick'
	, EN_STOP   => '_stop'
};

# These are POE's event classes (types).  They often shadow the event
# names themselves, but they can encompass a large group of events.
# For example, ET_ALARM describes anything enqueued as by an alarm
# call.  Types are preferred over names because bitmask tests are
# faster than string equality tests.
# Used in Resources/Sessions
use constant {
	ET_POST     => 0x0001  # User events (posted, yielded).
	, ET_CALL   => 0x0002  # User events that weren't enqueued.
	, ET_START  => 0x0004  # _start
	, ET_STOP   => 0x0008  # _stop
	, ET_SIGNAL => 0x0010  # _signal
	, ET_GC     => 0x0020  # _garbage_collect
	, ET_PARENT => 0x0040  # _parent
	, ET_CHILD  => 0x0080  # _child
	, ET_SCPOLL => 0x0100  # _sigchild_poll
	, ET_ALARM  => 0x0200  # Alarm events.
	, ET_SELECT => 0x0400  # File activity events.
	, ET_STAT   => 0x0800  # Statistics gathering
	, ET_SIGCLD => 0x1000  # sig_child() events.
};

# A mask for all events generated by/for users.
use constant ET_MASK_USER => ~(ET_GC | ET_SCPOLL | ET_STAT);

# These are ways a child may come or go.
# TODO - It would be useful to split 'lose' into two types.  One to
# indicate that the child has stopped, and one to indicate that it was
# given away.
# Used in Resources/Sessions
use constant {
	CHILD_GAIN     => 'gain'   # The session was inherited from another.
	, CHILD_LOSE   => 'lose'   # The session is no longer this one's child.
	, CHILD_CREATE => 'create' # The session was created as a child of this.
};

1;