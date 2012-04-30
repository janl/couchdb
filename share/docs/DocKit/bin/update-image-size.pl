#! /usr/bin/perl -w
# vim:set ts=2 sw=2 expandtab:

# update-image-size.pl
# 
# Parses DocBook XML and finds and sets the content height/width on images

# Martin MC Brown
# mc.brown@sun.com
# 2006-06-29

use strict;
use IO::File;
use Image::Info qw/image_info/;;
use File::Basename;
use Getopt::Long;
use Cwd;
use DateTime;

my $dt = DateTime->now();

my $opt_fixhtml = undef;

GetOptions(
           'fixhtml' => \$opt_fixhtml,
    );

my $dir = getcwd();

my $dqbase = (split(m{/},$dir))[-2];

foreach my $sourcefile (@ARGV)
{
    my $destfile = "$sourcefile.tmp";
    my $fileid = $sourcefile;
    $fileid =~ s/\.html$//g;
    
    my $dirname = dirname($sourcefile);

    my $ifh = new IO::File("$sourcefile") or die "Error: Can't open source file $sourcefile ($!)\n";
    $ifh->binmode(':utf8');
    my $text = join('',<$ifh>);
    $ifh->close();

# Skip the file has been marked for no image sizing updates

    if ($text =~ m/<!-- NOIMAGESIZEUPDATE -->/)
    {
        next;
    }

    my @imgs = ($text =~ m{(<imagedata.*?/>)}msg);
    my @htmlimgs = ($text =~ m{(<img.*?>)}msg);

    foreach my $img (@imgs)
    {
        my $newimg = $img;
        my ($fileref) = ($img =~ m/fileref="(.*?)"/);
        
        my $image = image_info("$dirname/$fileref");
        if (!defined($image) or $image->{error})
        {
            print STDERR "Couldn't parse image info for $fileref (within $sourcefile)\n";
        }
        else
        {
            my $imagewidth = $image->{width};
            my $imageheight = $image->{height};
            my $ratio = ($imagewidth/$imageheight);
            
            $newimg =~ s/(contentdepth=".*?")|(contentwidth=".*?")|(width=".*?")|(depth=".*?")|(scalefit=".*?")|(  +)//g;
            
            if ($newimg !~ m/(scalefit=".*?")/)
            {
                $newimg =~ s/<imagedata/<imagedata scalefit="1" /;
            }
            
            if ($newimg =~ m/(contentdepth=".*?")/)
            {
                $newimg =~ s/contentdepth=".*?"/contentdepth="100%" /;
            }
            else
            {
                $newimg =~ s/<imagedata/<imagedata contentdepth="100%" /;
            }
            
            if ($newimg =~ m/(width=".*?")/)
            {
                $newimg =~ s/width=".*?"/width="100%" /;
            }
            else
            {
                $newimg =~ s/<imagedata/<imagedata width="100%" /;
            }
            
            $newimg =~ s/(  +)/ /g;
            
            my $repl = quotemeta($img);
            $text =~ s{$repl}{$newimg}msg;
        }
    }

     foreach my $img (@htmlimgs)
    {
        my $newimg = $img;
        next if ($img =~ m/logo.png/);
        
        if ($newimg =~ m/(width=".*?")/)
        {
            $newimg =~ s/width=".*?"/width="100%"/;
        }
        else
        {
            $newimg =~ s/<img/<img width="100%" /;
        }
        
        if ($newimg =~ m/(height=".*?")/)
        {
            $newimg =~ s/height=".*?"/height="100%" /;
        }
        else
        {
            $newimg =~ s/<img/<img height="100%" /;
        }
        
        $newimg =~ s/(  +)/ /g;
        
        my $repl = quotemeta($img);
        $text =~ s{$repl}{$newimg}msg;
    }

# Replace the __DOCID__ with out document ID

    $text =~ s/__DOCID__/$dqbase/;

# Replace the HTML Meta build date

    my $builddate = sprintf('%04d-%02d-%02dT%02d:%02d:%02d+00:00',
                            $dt->year,
                            $dt->month,
                            $dt->day,
                            $dt->hour,
                            $dt->minute,
                            $dt->second,
        );

    if (defined($opt_fixhtml) &&
        $opt_fixhtml == 1)
    {
        $text =~ s/__meta_html_builddate__/$builddate/msg;
    }

    my $ofh = new IO::File($destfile,'w') or die "Error: Can't open destination file $destfile ($!)\n";
    $ofh->binmode(':utf8');
    print $ofh $text;
    $ofh->close();
    
    unlink($sourcefile);
    rename($destfile,$sourcefile);
}
