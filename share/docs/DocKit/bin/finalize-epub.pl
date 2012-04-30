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

my $couchdocs = new CouchDocs($global_opts);

# finalize-epub.pl - Finalizes an EPUB document

my $epubdir = shift or die "EPUB files directory must be specified";

my $contentfile = "$epubdir/OEBPS/content.opf";

my $content = $couchdocs->slurp_file($contentfile);

# Forcibly add a better Creator string

$content =~ s{</dc:title>}{</dc:title>\n    <dc:creator xmlns:dc="http://purl.org/dc/elements/1.1/">Apache, Inc.</dc:creator>};

# Fix the image references in the content file

$content =~ s{images/(published|source|generated)/}{images/}g;

# Look for a cover image, and then add it if it exists

if (-f "$epubdir/OEBPS/images/epub-logo.png")
{
    $content =~ s{<manifest>}{<manifest>\n      <item id="cover-image" href="images/epub-logo.png" media-type="image/png"/>};
}

$couchdocs->write_file($contentfile,$content);
