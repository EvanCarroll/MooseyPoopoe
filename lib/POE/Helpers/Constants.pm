package POE::Helpers::Constants;

use Sub::Exporter -setup => {
	exports => [ qw(
		MODE_RD MODE_WR MODE_EX

		EN_CHILD EN_GC EN_PARENT EN_SCPOLL EN_SIGNAL
		EN_START EN_STAT EN_STOP EN_DEFAULT
	
		CHILD_GAIN CHILD_LOSE CHILD_CREATE

		ET_POST  ET_CALL  ET_START  ET_STOP  ET_SIGNAL
		ET_GC  ET_PARENT  ET_CHILD  ET_SCPOLL  ET_ALARM
		ET_SELECT  ET_STAT  ET_SIGCLD  ET_MASK_USER

		EV_SESSION EV_SOURCE EV_NAME EV_TYPE EV_ARGS
		EV_OWNER_FILE EV_OWNER_LINE EV_TIME EV_SEQ

		TRACE_DEFAULT TRACE_EVENTS TRACE_FILES TRACE_PROFILE TRACE_REFCNT
		TRACE_RETVALS TRACE_SESSIONS TRACE_SIGNALS TRACE_STATISTICS TRACE_DESTROY

		ASSERT_DEFAULT ASSERT_DATA ASSERT_EVENTS ASSERT_FILES ASSERT_RETVALS
		ASSERT_USAGE ASSERT_STATES

		CATCH_EXCEPTIONS
		CHILD_POLLING_INTERVAL
		USE_SIGCHLD
		RUNNING_IN_HELL

		KR_SESSIONS KR_FILENOS KR_SIGNALS KR_ALIASES KR_ACTIVE_SESSION
		KR_QUEUE KR_ID KR_SESSION_IDS KR_SID_SEQ KR_EXTRA_REFS
		KR_SIZE KR_RUN KR_ACTIVE_EVENT KR_PIDS
	) ]

	, groups => {
		filehandle => [qw( MODE_RD MODE_WR MODE_EX )]
		, events   => [qw(
			EN_CHILD EN_GC EN_PARENT EN_SCPOLL EN_SIGNAL
			EN_START EN_STAT EN_STOP EN_DEFAULT
		
			ET_POST  ET_CALL  ET_START  ET_STOP  ET_SIGNAL
			ET_GC  ET_PARENT  ET_CHILD  ET_SCPOLL  ET_ALARM
			ET_SELECT  ET_STAT  ET_SIGCLD  ET_MASK_USER
		)]
		, event_index => [qw(
			EV_SESSION EV_SOURCE EV_NAME EV_TYPE EV_ARGS
			EV_OWNER_FILE EV_OWNER_LINE EV_TIME EV_SEQ
		)]
		, child    => [qw(
			CHILD_GAIN CHILD_LOSE CHILD_CREATE
		)]
		, poe_trace => [qw(
				POE_TRACE_EVENTS
				POE_TRACE_FILES
				POE_TRACE_PROFILE
				POE_TRACE_REFCNT
				POE_TRACE_RETVALS
				POE_TRACE_SESSIONS
				POE_TRACE_SIGNALS
				POE_TRACE_STATISTICS
		)]
		, kernel_index => [qw(
			KR_SESSIONS KR_FILENOS KR_SIGNALS KR_ALIASES KR_ACTIVE_SESSION
			KR_QUEUE KR_ID KR_SESSION_IDS KR_SID_SEQ KR_EXTRA_REFS
			KR_SIZE KR_RUN KR_ACTIVE_EVENT KR_PIDS
		)]
	}
};

use constant {
	## Filehandle activity modes.
	## They are often used as list indexes.
	## Used in Resources/ and Loops/
	MODE_RD   => 0 # read
	, MODE_WR => 1 # write
	, MODE_EX => 2 # exception/expedite

	## These are the names of POE's internal events.
	## Used in Resources/Sessions
	, EN_CHILD   => '_child'
	, EN_GC      => '_garbage_collect'
	, EN_PARENT  => '_parent'
	, EN_SCPOLL  => '_sigchld_poll'
	, EN_SIGNAL  => '_signal'
	, EN_DEFAULT => '_default'
	, EN_START   => '_start'
	, EN_STAT    => '_stat_tick'
	, EN_STOP    => '_stop'
	
	## EVENTS themselves
	, EV_SESSION    => 0  # $destination_session,
	, EV_SOURCE     => 1  # $sender_session,
	, EV_NAME       => 2  # $event_name,
	, EV_TYPE       => 3  # $event_type,
	, EV_ARGS       => 4  # \@event_parameters_arg0_etc,
	# (These fields go towards the end because they are optional in some cases
	# TODO: Is this still true?)
	, EV_OWNER_FILE => 5  # $caller_filename_where_enqueued,
	, EV_OWNER_LINE => 6  # $caller_line_where_enqueued,
	, EV_TIME       => 7  # Maintained by POE::Queue (create time)
	, EV_SEQ        => 8  # Maintained by POE::Queue (unique event ID)

	## These are POE's event classes (types).  They often shadow the event
	## names themselves, but they can encompass a large group of events.
	## For example, ET_ALARM describes anything enqueued as by an alarm
	## call.  Types are preferred over names because bitmask tests are
	## faster than string equality tests.
	## Used in Resources/Sessions
	, ET_POST   => 0x0001  # User events (posted, yielded).
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

	# These are ways a child may come or go.
	# TODO - It would be useful to split 'lose' into two types.  One to
	# indicate that the child has stopped, and one to indicate that it was
	# given away.
	# Used in Resources/Sessions
	, CHILD_GAIN   => 'gain'   # The session was inherited from another.
	, CHILD_LOSE   => 'lose'   # The session is no longer this one's child.
	, CHILD_CREATE => 'create' # The session was created as a child of this.
	
	# Kernel structure.  This is the root of a large data tree.  Dumping
	# $poe_kernel with Data::Dumper or something will show most of the
	# data that POE keeps track of.  The exceptions to this are private
	# storage in some of the leaf objects, such as POE::Wheel.  All its
	# members are described in detail further on.
	, KR_SESSIONS         =>  0 # \%kr_sessions,
	, KR_FILENOS        =>  1 # \%kr_filenos,
	, KR_SIGNALS        =>  2 # \%kr_signals,
	, KR_ALIASES        =>  3 # \%kr_aliases,
	, KR_ACTIVE_SESSION =>  4 # \$kr_active_session,
	, KR_QUEUE          =>  5 # \$kr_queue,
	, KR_ID             =>  6 # $unique_kernel_id,
	, KR_SESSION_IDS    =>  7 # \%kr_session_ids,
	, KR_SID_SEQ        =>  8 # \$kr_sid_seq,
	, KR_EXTRA_REFS     =>  9 # \$kr_extra_refs,
	, KR_SIZE           => 10 # XXX UNUSED ???
	, KR_RUN            => 11 # \$kr_run_warning
	, KR_ACTIVE_EVENT   => 12 # \$kr_active_event
	, KR_PIDS           => 13 # \%kr_pids_to_events

	, RUNNING_IN_HELL   => ($^O eq 'MSWin32' ? 1 : 0)

};

# A mask for all events generated by/for users.
use constant ET_MASK_USER => ~(ET_GC | ET_SCPOLL | ET_STAT);


use constant {
	CATCH_EXCEPTIONS => (
		defined($ENV{POE_CATCH_EXCEPTIONS})
		? $ENV{POE_CATCH_EXCEPTIONS} : 1 # per old Kernel.pm
	)
	, CHILD_POLLING_INTERVAL => (
		defined($ENV{POE_CHILD_POLLING_INTERVAL})
		? $ENV{POE_CHILD_POLLING_INTERVAL} : 1 # one sec not true, per old Kernel
	)
	, USE_SIGCHLD => (
		defined($ENV{POE_USE_SIGCHLD})
		? $ENV{POE_USE_SIGCHLD} : 0 # per old Kernel.pm
	)
	, EVENT_LOOP => (
		defined($ENV{POE_EVENT_LOOP})
		? $ENV{POE_EVENT_LOOP} : ASSERT_DEFAULT
	)
	, TRACE_DEFAULT  => $ENV{POE_TRACE_DEFAULT}
	, ASSERT_DEFAULT => $ENV{POE_ASSERT_DEFAULT}
};

use constant {
	TRACE_EVENTS => (
		defined($ENV{POE_TRACE_EVENTS})
		? $ENV{POE_TRACE_EVENTS} : TRACE_DEFAULT
	)
	, TRACE_FILES => (
		defined($ENV{POE_TRACE_FILES})
		? $ENV{POE_TRACE_FILES} : TRACE_DEFAULT
	)
	, TRACE_PROFILE => (
		defined($ENV{POE_TRACE_PROFILE})
		? $ENV{POE_TRACE_PROFILE} : TRACE_DEFAULT
	)
	, TRACE_REFCNT => (
		defined($ENV{POE_TRACE_REFCNT})
		? $ENV{POE_TRACE_REFCNT} : TRACE_DEFAULT
	)
	, TRACE_RETVALS => (
		defined($ENV{POE_TRACE_RETVALS})
		? $ENV{POE_TRACE_RETVALS} : TRACE_DEFAULT
	)
	, TRACE_SESSIONS => (
		defined($ENV{POE_TRACE_SESSIONS})
		? $ENV{POE_TRACE_SESSIONS} : TRACE_DEFAULT
	)
	, TRACE_SIGNALS => (
		defined($ENV{POE_TRACE_SIGNALS})
		? $ENV{POE_TRACE_SIGNALS} : TRACE_DEFAULT
	)
	, TRACE_STATISTICS => (
		defined($ENV{POE_TRACE_STATISTICS})
		? $ENV{POE_TRACE_STATISTICS} : TRACE_DEFAULT
	)
	, TRACE_DESTROY => (
		defined($ENV{POE_TRACE_DESTROY})
		? $ENV{POE_TRACE_DESTROY} : TRACE_DEFAULT
	)

	, ASSERT_DATA => (
		defined($ENV{POE_ASSERT_DATA})
		? $ENV{POE_ASSERT_DATA} : ASSERT_DEFAULT
	)
	, ASSERT_EVENTS => (
		defined($ENV{POE_ASSERT_EVENTS})
		? $ENV{POE_ASSERT_EVENTS} : ASSERT_DEFAULT
	)
	, ASSERT_FILES => (
		defined($ENV{POE_ASSERT_FILES})
		? $ENV{POE_ASSERT_FILES} : ASSERT_DEFAULT
	)
	, ASSERT_RETVALS => (
		defined($ENV{POE_ASSERT_RETVALS})
		? $ENV{POE_ASSERT_RETVALS} : ASSERT_DEFAULT
	)
	, ASSERT_USAGE => (
		defined($ENV{POE_ASSERT_USAGE})
		? $ENV{POE_ASSERT_USAGE} : ASSERT_DEFAULT
	)
	, ASSERT_STATES => (
		defined($ENV{POE_ASSERT_STATES})
		? $ENV{POE_ASSERT_STATES} : ASSERT_DEFAULT
	)
};

1
