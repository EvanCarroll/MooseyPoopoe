#!/usr/bin/env perl
# $Id: svn-log.perl 36 2007-08-13 06:39:26Z rcaputo $

# This program is Copyright 2005 by Rocco Caputo.  All rights are
# reserved.  This program is free software.  It may be modified, used,
# and redistributed under the same terms as Perl itself.

# Generate a nice looking change log from the subversion logs for a
# Perl project.  The log is also easy for machines to parse.

use warnings;
use strict;

use Getopt::Long;
use Text::Wrap qw(wrap fill $columns $huge);
use POSIX qw(strftime);
use XML::Parser;
use XML::LibXML;

use constant DEBUG => 1;

my %month = qw(
	Jan 01 Feb 02 Mar 03 Apr 04 May 05 Jun 06
	Jul 07 Aug 08 Sep 09 Oct 10 Nov 11 Dec 12
);

$Text::Wrap::huge     = "wrap";
$Text::Wrap::columns  = 74;

my $days_back  = 365;   # Go back a year by default.
my $send_help  = 0;     # Display help and exit.
my $svn_repo;           # Where to log from.
my $tag_pattern = "^v\\d+_";

use constant LOG_REV        => 0;
use constant LOG_DATE       => 1;
use constant LOG_WHO        => 2;
use constant LOG_MESSAGE    => 3;
use constant LOG_PATHS      => 4;

use constant PATH_PATH      => 0;
use constant PATH_ACTION    => 1;
use constant PATH_CPF_PATH  => 2;
use constant PATH_CPF_REV   => 3;

use constant TAG_REV        => 0;
use constant TAG_TAG        => 1;
use constant TAG_LOG        => 2;

use constant MAX_TIMESTAMP  => "9999-99-99 99:99:99.999999Z";

GetOptions(
	"age=s"      => \$days_back,
	"repo=s"     => \$svn_repo,
	"help"       => \$send_help,
	"tags=s"     => \$tag_pattern,
) or exit;

# Find the trunk for the current repository if one isn't specified.
unless (defined $svn_repo) {
	$svn_repo = `svn info . | grep '^URL: '`;
	if (length $svn_repo) {
		chomp $svn_repo;
		$svn_repo =~ s{^URL\:\s+(.+?)/trunk/?.*$}{$1};
	}
	else {
		$send_help = 1;
	}
}

die(
	"$0 usage:\n",
	"  --repo URL  where to find the repository\n",
	"  [--age DAYS]  limit report to DAYS in the past (default: 365)\n",
	"  [--tags REGEXP]  report on tags matching REGEXP (default: ^v\\d+_)\n",
	"\n",
	"REPOSITORY must have a trunk subdirectory and a tags directory where\n",
	"release tags are kept.\n",
) if $send_help;

my $earliest_date = strftime(
	"%FT%T.000000Z", gmtime(time() - $days_back * 86400)
);
DEBUG and warn "earliest date = $earliest_date\n";

### 1. Gather a list of tags for the repository, their revisions and
### dates.

my %tag;

my $parser = XML::LibXML->new();

my $doc = $parser->parse_string(scalar `svn --xml list $svn_repo/tags`);

sub get_value_of_element {
	my $element = shift;
	return "" unless defined $element;
	my $child = $element->getFirstChild();
	return "" unless defined $child;
	my $value = $child->getData();
	return "" unless defined $value;
	$value =~ s/\s+/ /g;
	$value =~ s/^\s//;
	$value =~ s/\s$//;
	return $value;
}

#<entry kind="dir">
#	<name>docs-wiki-start</name>
#	<commit revision="1901">
#		<author>rcaputo</author>
#		<date>2006-03-20T02:54:27.572888Z</date>
#	</commit>
#</entry>

foreach my $entry ($doc->getElementsByTagName("entry")) {
	next unless $entry->getAttribute("kind") eq "dir";

	my $tag = get_value_of_element(($entry->getChildrenByTagName("name"))[0]);

	my ($rev, $author, $date);
	foreach my $commit ($entry->getElementsByTagName("commit")) {
		$rev = $commit->getAttribute("revision");
		$author = get_value_of_element(
			($commit->getChildrenByTagName("author"))[0]
		);
		$date = get_value_of_element(
			($commit->getChildrenByTagName("date"))[0]
		);
	}

	next unless $tag =~ /$tag_pattern/o;

	DEBUG and warn "rev($rev) date($date) tag($tag)\n";

	$tag{$date} = [
		$rev,  # TAG_REV
		$tag,  # TAG_TAG
		[ ],   # TAG_LOG
	];
}

# Fictitious "HEAD" tag for revisions that came after the last tag.

$tag{+MAX_TIMESTAMP} = [
	"HEAD",         # TAG_REV
	"(untagged)",   # TAG_TAG
	undef,          # TAG_LOG
];

### 2. Gather the log for the current directory.  Store log entries
### under their proper tags.

my @tag_dates = sort keys %tag;
while (my $date = pop(@tag_dates)) {

	# We're done if this date's before our earliest date.
	if ($date lt $earliest_date) {
		delete $tag{$date};
		next;
	}

	my $tag = $tag{$date}[TAG_TAG];
	DEBUG and warn "Gathering information for tag $tag...\n";

	my $this_rev = $tag{$date}[TAG_REV];
	my $prev_rev;
	if (@tag_dates) {
		$prev_rev = $tag{$tag_dates[-1]}[TAG_REV];
	}
	else {
		$prev_rev = 0;
	}

	DEBUG and warn "$this_rev:$prev_rev";
	my @log = gather_log(".", "-r", "$this_rev:$prev_rev");

	$tag{$date}[TAG_LOG] = \@log;
}

### 3. PROFIT!  No, wait... generate the nice log file.

foreach my $timestamp (sort { $b cmp $a } keys %tag) {
	last if $timestamp lt $earliest_date;
	my $tag_rec = $tag{$timestamp};

	# Skip this tag if there are no log entries.
	next unless @{$tag_rec->[TAG_LOG]};

	my $tag_line = "$timestamp $tag_rec->[TAG_TAG]";
	my $tag_bar  = "=" x length($tag_line);
	print $tag_bar, "\n", $tag_line, "\n", $tag_bar, "\n\n";

	foreach my $log_rec (@{$tag_rec->[TAG_LOG]}) {

		my @paths = @{$log_rec->[LOG_PATHS]};
		if (@paths > 1) {
			@paths = grep {
				$_->[PATH_PATH] ne "/trunk" or $_->[PATH_ACTION] ne "M"
			} @paths;
		}

		my $time_line = wrap(
			"  ", "  ",
			join(
				"; ",
				"$log_rec->[LOG_DATE] (r$log_rec->[LOG_REV]) by $log_rec->[LOG_WHO]",
				map { "$_->[PATH_PATH] $_->[PATH_ACTION]" } @paths
			)
		);

		if ($time_line =~ /\n/) {
			$time_line = wrap(
				"  ", "  ",
				"$log_rec->[LOG_DATE] (r$log_rec->[LOG_REV]) by $log_rec->[LOG_WHO]\n"
			) .
			wrap(
				"  ", "  ",
				join(
					"; ",
					map { "$_->[PATH_PATH] $_->[PATH_ACTION]" } @paths
				)
			);
		}

		print $time_line, "\n\n";

		# Blank lines should have the indent level of whitespace.  This
		# makes it easier for other utilities to parse them.

		my @paragraphs = split /\n\s*\n/, $log_rec->[LOG_MESSAGE];
		foreach my $paragraph (@paragraphs) {

			# Trim off identical leading space from every line.
			my ($whitespace) = $paragraph =~ /^(\s*)/;
			if (length $whitespace) {
				$paragraph =~ s/^$whitespace//mg;
			}

			# Re-flow the paragraph if it isn't indented from the norm.
			# This should preserve indented quoted text, wiki-style.
			unless ($paragraph =~ /^\s/) {
				$paragraph = fill("    ", "    ", $paragraph);
			}
		}

		print join("\n    \n", @paragraphs), "\n\n";
	}
}

print(
	"==============\n",
	"End of Excerpt\n",
	"==============\n",
);

### Z. Helper functions.

sub gather_log {
	my ($url, @flags) = @_;

	my (@log, @stack);

	my $parser = XML::Parser->new(
		Handlers => {
			Start => sub {
				my ($self, $tag, %att) = @_;
				push @stack, [ $tag, \%att ];
				if ($tag eq "logentry") {
					push @log, [ ];
					$log[-1][LOG_WHO] = "(nobody)";
				}
			},
			Char  => sub {
				my ($self, $text) = @_;
				$stack[-1][1]{0} .= $text;
			},
			End => sub {
				my ($self, $tag) = @_;
				die "close $tag w/out open" unless @stack;
				my ($pop_tag, $att) = @{pop @stack};

				die "$tag ne $pop_tag" if $tag ne $pop_tag;

				if ($tag eq "date") {
					my $timestamp = $att->{0};
					my ($date, $time) = split /[T.]/, $timestamp;
					$log[-1][LOG_DATE] = "$date $time";
					return;
				}

				if ($tag eq "logentry") {
					$log[-1][LOG_REV] = $att->{revision};
					return;
				}

				if ($tag eq "msg") {
					$log[-1][LOG_MESSAGE] = $att->{0};
					return;
				}

				if ($tag eq "author") {
					$log[-1][LOG_WHO] = $att->{0};
					return;
				}

				if ($tag eq "path") {
					my $path = $att->{0};
					$path =~ s{^/trunk/}{};
					push(
						@{$log[-1][LOG_PATHS]}, [
							$path,            # PATH_PATH
							$att->{action},   # PATH_ACTION
						]
					);

					$log[-1][LOG_PATHS][-1][PATH_CPF_PATH] = $att->{"copyfrom-path"} if (
						exists $att->{"copyfrom-path"}
					);

					$log[-1][LOG_PATHS][-1][PATH_CPF_REV] = $att->{"copyfrom-rev"} if (
						exists $att->{"copyfrom-rev"}
					);
					return;
				}

			}
		}
	);

	my $cmd = "svn -v --xml @flags log $url";
	DEBUG and warn "Command: $cmd\n";

	open(LOG, "$cmd|") or die $!;
	$parser->parse(*LOG);
	close LOG;

	return @log;
}
