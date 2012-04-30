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

my ($srcfile) = (@ARGV);

croak("Must specify source files") unless(defined($srcfile));

my $couchdocs = new CouchDocs($global_opts);

$couchdocs->docmapparse($srcfile);

