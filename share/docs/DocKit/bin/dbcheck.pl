#! /usr/bin/perl -w

use strict;
use warnings;
use File::Basename;
BEGIN {
    use lib dirname($0);
}
use CouchDocs;
use CouchDocs::DBCheck;
use Getopt::Long;
use XML::Parser::PerlSAX;

my $global_opts = {};
GetOptions("opt=s%" => \$global_opts);

my ($srcfile) = (@ARGV);

my $couchdocs = new CouchDocs($global_opts);

if (exists($global_opts->{checkulink}) && $global_opts->{checkulink} == 1)
{
    print STDERR "\nWarning: Checking for external links may take some time. Please be patient\n";

    eval "require LWP::UserAgent;";

    die "ulink checking requires LWP::UserAgent\n"
        if ($@);
}

my $db_handler = CouchDocs::DBCheck->new($global_opts);
my $src = $couchdocs->slurp_file($srcfile);

XML::Parser::PerlSAX->new->parse(Source => { String => $src}, 
                                 Handler => $db_handler);

foreach my $item (@{$db_handler->{issuelist}})
{
    print "Error in $item->{parentid}:\n\t$item->{text}";
}
