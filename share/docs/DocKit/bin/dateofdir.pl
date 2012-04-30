#!/usr/bin/perl

use warnings;
use strict;
use File::Basename;
use Carp;
BEGIN {
    use lib dirname($0);
}
use DateTime;

my $maxdate = 0;

foreach my $file (@ARGV)
{
    next if ($file =~ m{/common/});
    my $filedate = (stat $file)[9];

    $maxdate = $filedate if ($filedate > $maxdate);
}

my $date = DateTime->from_epoch(epoch => $maxdate);

printf("%s %s\n",$date->dmy('/'),$date->hms);
