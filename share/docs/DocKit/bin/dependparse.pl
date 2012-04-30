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

my ($srcfile,$dependfile) = (@ARGV);

croak("Must specify source/destination files") unless(defined($srcfile));

my $couchdocs = new CouchDocs($global_opts);

my $realfile = $srcfile;

if ($global_opts->{buildtype} eq 'version')
{
    $realfile = sprintf('%s/%s',$global_opts->{buildsrc},$srcfile);
}

$couchdocs->dependparse($realfile,$srcfile);

my @dependfile = ();

foreach my $basefile (sort keys %{$couchdocs->{dependencies}})
{
    next if ($basefile =~ m/^_/);

    $couchdocs->{dependencies}->{$basefile}->{Makefile} = 1;

    my $srccopy = '';
    if ($global_opts->{buildtype} eq 'version')
    {
        my $origsrc = sprintf('%s/%s',$global_opts->{buildsrc},$basefile);

        if ((stat($origsrc))[1] !=
            (stat($basefile))[1])
        {
            $srccopy = sprintf("\t\$(VERSION_COPY) %s %s\n",
                               $origsrc,
                               $basefile);
            $couchdocs->{dependencies}->{$basefile}->{$origsrc} = 1;
        }
    }

    my @dependgraph = sort(dependloop($couchdocs,$basefile));

    push(@dependfile,
         sprintf("%s: %s\n%s\n",
                 $basefile,
                 join(" \\\n\t\t",@dependgraph),
                 $srccopy,
                 ),
        );

    my ($basename) = ($basefile);
    $basename =~ s{\.xml$}{};

    push(@dependfile,
         sprintf("%s-ready.xml: %s\n\n",
                 $basename,
                 join(" \\\n\t\t",@dependgraph),
                 ),
        );

    push(@dependfile,
         sprintf("%s-ready-online.xml: %s\n\n",
                 $basename,
                 join(" \\\n\t\t",@dependgraph),
                 ),
        );
}

my $basename = $srcfile;
$basename =~ s/\./_/g;

if ($global_opts->{buildtype} eq 'version')
{
    foreach my $imgfile (sort keys %{$couchdocs->{dependencies}->{_globalimages}})
    {
        push(@dependfile,
             sprintf("%s: \n\tmkdir -p images\n\t\$(VERSION_COPY) %s/%s %s\n\n",
                     $imgfile,
                     $global_opts->{buildsrc},
                     $imgfile,
                     $imgfile,
             )
            );
    }
}

push(@dependfile,
     sprintf("BASE_TARGET_IMAGES = %s\n\n",
             join(" \\\n\t",sort keys %{$couchdocs->{dependencies}->{_globalimages}}),
     )
    );

push(@dependfile,
     sprintf("XML_SOURCES = %s\n\n",
             join(" \\\n\t",sort keys %{$couchdocs->{dependencies}->{_xml}}),
     )
    );

push(@dependfile,
     sprintf("XML_BASE_SOURCES = %s\n\n",
             join(" \\\n\t",sort map { $_ =~ s/\.xml$//; $_ } keys %{$couchdocs->{dependencies}->{_xmlbase}}),
     )
    );


$couchdocs->write_file($dependfile,join('',@dependfile));

sub dependloop
{
    my ($couchdocs,$basefile) = @_;

    my @filelist = ();

    return(@filelist) unless (defined($couchdocs) && 
                   defined($basefile));
    return(@filelist) unless (exists($couchdocs->{dependencies}->{$basefile}));

    foreach my $file (keys %{$couchdocs->{dependencies}->{$basefile}})
    {
        next if ($file eq $basefile);
        push(@filelist,$file);
        push(@filelist,dependloop($couchdocs,$file));
    }

    my $unique = {};
    map { $unique->{$_} = 1 } @filelist;

    return(sort keys %{$unique});
}
