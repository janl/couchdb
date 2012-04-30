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

my ($srcfile,$outfile) = (@ARGV);

croak "Source file not found\n" unless(defined($srcfile) && (-f $outfile));
croak "Destination file not found\n" unless(defined($outfile) && (-f $outfile));

if ((stat($srcfile))[1] == (stat($outfile))[1])
{
    carp "Files are the same, skipping\n";
    exit(0);
}
