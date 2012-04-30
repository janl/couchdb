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

my ($metafile) = (@ARGV);
$metafile =~ s/\.xml//g;

croak("Must specify XML meta file") unless(defined($metafile));

my $couchdocs = new CouchDocs($global_opts);

$couchdocs->metavalidate($metafile);

