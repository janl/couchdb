#!/usr/bin/perl

use warnings;
use strict;
use File::Basename;
use Carp;
BEGIN {
    use lib dirname($0);
}

use CouchDocs;

use Getopt::Long;

my $global_opts = {};
GetOptions("opt=s%" => \$global_opts);

my ($srcfile,$destfile) = (@ARGV);

croak("Must specify source/destination files") unless(defined($srcfile) &&
                                                      defined($destfile));

my $couchdocs = new CouchDocs($global_opts);
$couchdocs->{tool_dependencies}->{$destfile}->{$0} = 1;

$couchdocs->metaparse($srcfile,$destfile);

