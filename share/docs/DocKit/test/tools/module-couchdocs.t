#!/usr/bin/perl

use warnings;
use strict;
use lib '../../bin';
use CouchDocs;
use Test::Simple tests => 12;

my $cd = new CouchDocs();

my $versions = { 
    '1.1.0' => 1001001000000,
    '0.9.3' => 1000009003000,
    '1.1.23' => 1001001023000,
    '4.0.27a' => 1004000027097,
};
  
foreach my $version (keys %{$versions})
{                   
    my $integer = $cd->vertodec($version);
    my $reverse = $cd->dectover($versions->{$version});
    my $full    = $cd->dectover($cd->vertodec($version));

    ok( $cd->vertodec($version) == $versions->{$version}, "vertodec $version");
    ok( "$reverse" eq "$version", "dectover $version");
    ok( $version eq $full, "vertodec(dectover) $version");
}

sub compare 
{
    my ($a,$b) = @_;

    my @achars = split //,$a;
    my @bchars = split //,$b;

    my $true = 1;

    while($true)
    {
        my $ac = pop @achars;
        my $bc = pop @bchars;
        last if (!defined($ac) || !defined($bc));

        print "Checking chars $ac $bc\n";
        printf("Checking %d %d\n",ord($ac),ord($bc));

    }

}
