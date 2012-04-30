package CouchDocs;

use Carp qw(croak cluck confess);
use File::Basename;
use IO::File;
use Data::Dumper;
use strict;
use warnings;
use Module::Load;
use Module::Util qw/:all/;
use File::Spec::Functions;
use DateTime;
use Cwd;

sub new
{
    my ($self,$opts) = @_;

    my $class = ref($self) || $self;

    return bless {
        _modulename => 'CouchDocs',
        global_opts => $opts,
        dependencies => {},
        module_map => {
            'urlapi' => 'CouchDocs::URLAPI',
            'json'   => 'CouchDocs::JSON',
            'config' => 'CouchDocs::Config',
        },
        module_cache => {
        },
        metatext => [],
    };
}

# Take a string, look for metadocs insertion statements, load the corresponding module
# and then parse and insert the content

sub metaparse
{
    my ($self,$srcfile,$destfile) = @_;

    my $srctext = $self->slurp_file($srcfile);

    my $blddestfile = $destfile;
    $blddestfile =~ s/\.tmp$//;

    $self->{dependencies}->{$blddestfile}->{$srcfile} = 1;
    $self->{tool_dependencies}->{$blddestfile}->{$self->resolve_module_path('CouchDocs')} = 1;

    my @metaelements;
    my ($protection_counter,$protection_max) = (0,10);

    @metaelements = get_meta_statements($srctext);

    while(scalar @metaelements > 0)
    {
        my $metaelement = pop @metaelements;

# Metasettings contain the specific info;

        my $metasettings = {};

        extract_meta_settings($metaelement,\$metasettings);

# If there is a version specification in the metasettings
# then check if the value is 'inherit', and use the line option

        if (exists($metasettings->{version}) &&
            $metasettings->{version} eq 'inherit')
        {
            $metasettings->{version} = $self->{global_opts}->{version};
        }

# There must be a meta type in the specification
        if (!exists($metasettings->{type}) &&
            !defined($metasettings->{type}))
        {
            croak("You must specify a meta-type");
        }

        my $type = $metasettings->{type};
        my $module = $self->{module_map}->{$type};

        if (!exists($self->{module_map}->{$type}) ||
            !defined($module))
        {
            $self->meta_error("Cannot find module for metadoc type $type\n");
        }

        if (!exists($self->{module_cache}->{$type}) &&
            !defined($self->{module_cache}->{$type}))
        {
            load $module;

            $self->{module_cache}->{$type} = eval "$module->new();";

            $self->{tool_dependencies}->{$blddestfile}->{$self->resolve_module_path($module)} = 1;

            croak "Couldn't initiate $self->{module_map}->{$type}"
                unless (defined($self->{module_cache}->{$type}));

            $self->{module_cache}->{$type}->set_global_opt($self->{global_opts});
        }

        my $response = $self->{module_cache}->{$type}->parse($metasettings);

        push(@metaelements,get_meta_statements($response->{text}));
        my $quotedelement = quotemeta($metaelement);

        my $replacetext = '';
        if (exists($response->{embedded_dependencies}) &&
            $response->{embedded_dependencies} == 1)
        {
            $replacetext = $response->{text};
            my $depend = join("\n",
                              $self->generate_dependencies('meta',$response->{dependencies}),
                              $self->generate_dependencies('tool',$self->{tool_dependencies}->{$blddestfile}));
            $replacetext =~ s/<!--DEPENDBLOCK-->/$depend/;
        }
        else
        {
            $replacetext = join("\n",
                                $self->generate_dependencies('meta',$response->{dependencies}),
                                $self->generate_dependencies('tool',$self->{tool_dependencies}->{$blddestfile}),
                                $response->{text});
        }

        $srctext =~ s/$quotedelement/$replacetext/msg;
    }

    return if (exists($self->{global_opts}->{depend}) &&
               ($self->{global_opts}->{depend} = 1));

    $srctext = $self->remap_dynxml_source($srctext);

    $self->write_file($destfile,$srctext);
}

# Validate a given XML source

sub metavalidate
{
    my ($self,$xmlfile) = @_;

    my $module =  $self->{module_map}->{$self->{global_opts}->{metatype}};

    load $module;

    my $validate  = eval "$module->new();";

    $validate->set_global_opt($self->{global_opts});

    $validate->validate($xmlfile);
}

sub remap_dynxml_source
{
    my ($self,$string) = @_;

    $string =~ s{(!#!amp!#!(.*?)!#!;!#!)}{&$2;}msg;

    return($string);
}

sub generate_dependencies
{
    my ($self,$type,$depends) = @_;

    my @dependlist;

    foreach my $file (keys %{$depends})
    {
        push(@dependlist,sprintf('<remark role="dependency-%s" condition="%s"/>',
                                 $type,
                                 $file));
    }
    return(@dependlist);
}

sub resolve_module_path
{
    my ($self,$module) = @_;

    my $dirname = dirname($0);
    $module =~ s{::}{/}g;
    $module .= '.pm';
    return "$dirname/$module";
}

# Perform any final clean up and formatting on a sourcefile

sub finalparse
{
    my ($self,$srcfile,$dstfile) = @_;

    my $filetext = $self->slurp_file($srcfile);

# Replace the metaversion tag with the version from the makefile

    $filetext =~ s/__meta_version__/$self->{global_opts}->{version}/msg;
    my $verid = $self->format_version_as_docbook($self->{global_opts}->{version});
    $filetext =~ s/__meta_version_id__/$verid/msg;

# Build and changes dates
    my $builddt = DateTime->from_epoch(epoch => $^T);
    my $builddate = sprintf('%d %s %d %d:%d',
                            $builddt->day,
                            $builddt->month_abbr,
                            $builddt->year,
                            $builddt->hour,
                            $builddt->minute);

    my $maxdate = 0;

    foreach my $file (split(/ /,$self->{global_opts}->{filelist}))
    {
        next if ($file =~ m{/common/});
        my $filedate = (stat $file)[9];
        
        $maxdate = $filedate
            if ($filedate > $maxdate);
    }

    $builddt = DateTime->from_epoch(epoch => $maxdate);
    my $changedate = sprintf('%02d %s %04d %02d:%02d',
                             $builddt->day,
                             $builddt->month_abbr,
                             $builddt->year,
                             $builddt->hour,
                             $builddt->minute);

    $filetext =~ s/__meta_builddate__/$builddate/msg;
    $filetext =~ s/__meta_changedate__/$changedate/msg;

# Remap any DynXML elements 

    $filetext = $self->remap_dynxml_source($filetext);

    $self->write_file($dstfile,$filetext);
}

# Dependency parser
# This takes the information populated during parsing metadocs and
# individual modules (which is itself tracked by adding the file
# names/paths into a hash structure
# The code also has to follow any imports or requirements loaded from
# other files
# We also have to handle the fact that certain files are essentially
# global dependencies for the root target (for example, image files)
# This is because we must be able to determine a list of image files
# to be copied into target directories

sub dependparse
{
    my ($self,$filename,$parentfile) = @_;
    $parentfile = $filename if (!defined($parentfile));

    my @dlist; 

    push(@dlist,
         {
             realfile => $filename,
             srcfile => $parentfile
         });

    while (my $dlistentry = pop @dlist)
    {
        my ($realfile,$srcfile) = ($dlistentry->{realfile},
                                   $dlistentry->{srcfile});

        next if (exists($self->{dependencyparsed}->{$realfile}));

        if (!-f $realfile)
        {
            my $build_command = sprintf('%s --directory %s %s',
                                        $ENV{MAKE} || 'make',
                                        dirname($realfile),
                                        basename($realfile)
                );
            system($build_command);
            croak "Couldn't find/build $srcfile: $?"
                if (!-f $srcfile);
        }

        $self->{dependencies}->{$srcfile} = {} unless (exists($self->{dependencies}->{$srcfile}));
        $self->{dependencies}->{$srcfile}->{$realfile} = 1;
        
        $self->{dependencies}->{_xml}->{catfile($self->{global_opts}->{buildsrc},$realfile)} = 1;
        
        if ($realfile !~ m/metadoc-(.*?).xml/)
        {
            $self->{dependencies}->{_xmlbase}->{catfile($self->{global_opts}->{buildsrc},$realfile)} = 1;
        }
        
        my $filetext = $self->slurp_file($realfile);
        
# Pick out dependency remarks
        
        my (@remarks) = ($filetext =~ m/<remark role="dependency-tool" condition="([^"]+)"\/>/msg);
        
        foreach my $remark (@remarks)
        {
            $self->{dependencies}->{catfile($self->{global_opts}->{buildsrc},dirname($srcfile),$realfile)}->{$remark} = $remark;
        }
        (@remarks) = ($filetext =~ m/<remark role="dependency-source" condition="([^"]+)"\/>/msg);
        
        foreach my $remark (@remarks)
        {
            $self->{dependencies}->{catfile($self->{global_opts}->{buildsrc},dirname($srcfile),$realfile)}->{$remark} = $remark;
            
            push(@dlist,
                 { 
                     realfile => catfile($self->{global_opts}->{buildsrc},dirname($realfile),$remark),
                     srcfile => $remark
                 }
                );
        }
        (@remarks) = ($filetext =~ m/<remark role="dependency-meta" condition="([^"]+)"\/>/msg);
        
        foreach my $remark (@remarks)
        {
            $self->{dependencies}->{catfile($self->{global_opts}->{buildsrc},dirname($srcfile),$realfile)}->{$remark} = $remark;
        }
        
# Pick out xi:include
# An xinclude of a metadoc requires us to parse the -metasrc file, not the metadoc- file

        my (@includes) = ($filetext =~ m/xi:include.*?href="([^"]+)"/msg);

        foreach my $includesrc (@includes)
        {
            if ($includesrc =~ m/metadoc-(.*?).xml/)
            {
                my $sourcefile = catfile($self->{global_opts}->{buildsrc},
                                         dirname($realfile),
                                         dirname($includesrc),
                                         sprintf('%s-metasrc.xml',$1));
                $self->{dependencies}->{_xml}->{$sourcefile} = 1;
                $self->{dependencies}->{_xmlbase}->{$sourcefile} = 1;
            }
            # Parse the extracted include file ($include), using the directory of the file to parse ($realfile) and the
            # current build directory, with $realfile as the parent
            
            my $includefile = catfile($self->{global_opts}->{buildsrc},dirname($realfile),$includesrc);
            
            push(@dlist,
                 {
                     realfile => $includefile,
                     srcfile => $realfile
                 }
                );
        }
        
# Pick out images
        
        my (@images) = $filetext =~ m/<imagedata.*?fileref="([^"]+)"/msg;
        
        foreach my $image (@images)
        {
            my $imageloc = catfile($self->{global_opts}->{buildsrc},dirname($realfile),$image);
            $self->{dependencies}->{$srcfile}->{$imageloc} = 1;
            $self->{dependencies}->{_globalimages}->{$imageloc} = 1;
        }
        
        $self->{dependencyparsed}->{$realfile} = 1;
    }
}

# Copies the global options through into the object

sub set_global_opt
{
    my ($self,$options) = @_;

    $self->{global_opts} = $options;
}

# meta_object_filter takes an object and settings specification
# and returns true or false based on whether all the filter specifications
# and object entries match
#
# We also check the settings match the version

sub meta_object_filter
{
    my ($self,$settings,$object) = @_;

# Check the versions first

    if (exists($settings->{version}))
    {
        if ($self->meta_verify_version($settings->{version},
                                       $object) == 0)
        {
            return(0);
        }
    }

# Filters are specified in the metasettings structure (as read
# from the remarks) using the filter_ prefix
# the suffix in each case should be a hash entry in the supplied
# object against which we can match

    my $filtercount = 0;
    my $matchcount = 0;

    foreach my $id (keys %{$settings})
    {
        next unless ($id =~ m/^filter_(.*)/);
        my $filterfield = $1;
        $filtercount++;

# If the filter field doesn't exist, then we can't filter it (so it's valid)

        if (exists($object->{$filterfield}))
        {
            if (ref($object->{$filterfield}) eq 'HASH')
            {
                $matchcount++
                    if (exists($object->{$filterfield}->{$settings->{$id}}));
            }
            else
            {
                $matchcount++
                    if ($object->{$filterfield} eq $settings->{$id});
            }
        }
        else
        {
            $matchcount++;
        }
    }
    if ($filtercount == $matchcount)
    {
        return(1);
    }
    return(0);
}

# meta_verify_version checks the following tags in a structure
# and returns true or false if the version spec matches the
# supplied version. Tags are:
#
# iv (in version) - the version where the item was first available
# ov (out version) - the version where the item became unavilable
# rv (deleted version) - the version when it was removed (implies ov)
# dv (deprecated version) - still available but not recommended

sub meta_verify_version
{
    my ($self,$checkversion,$checkobject) = @_;

# Default is for the item not to match

    my $available = 0;

# Versions are inclusive, and can include either a range
# or an explicit list of versions

    my ($lowerversion,$upperversion,@versionlist);

    if ($checkversion =~ m/,/)
    {
        @versionlist = split(/,/,$checkversion);
    }
    else
    {
        push @versionlist,$checkversion;
    }

    foreach my $version (@versionlist)
    {
        if ($version =~ m/(.*)-(.*)/)
        {
            ($lowerversion,$upperversion) = ($1,$2);
        }
        else
        {
            ($lowerversion,$upperversion) = ($version,$version);
        }
        if ($self->meta_verify_version_explicit($lowerversion,$upperversion,$checkobject) == 1)
        {
            $available = 1;
        }
    }
    return($available);
}

# Verify an explicit version

sub meta_verify_version_explicit
{
    my ($self,$lowerversion,$upperversion,$checkobject) = @_;

     my $available = 1;

# If we dont have an in or out version, short circuit a valid response

    if (!exists($checkobject->{iv}) &&
        !exists($checkobject->{ov}) &&
        !defined($checkobject->{iv}) &&
        !defined($checkobject->{ov})
        )
    {
        return $available;
    }

# If the upper version is less than the inversion, 
# then we haven't made it yet, ergo no match

    if ((exists($checkobject->{iv})) &&
        ($self->vertodec($upperversion) <
         $self->vertodec($checkobject->{iv})))
    {
        $available = 0;
    }

# If the lowerversion is greater than the outversion,
# then we are beyond the version, ergo no match

    if ((exists($checkobject->{ov})) &&
        ($self->vertodec($lowerversion) >=
         $self->vertodec($checkobject->{ov})))
    {
        $available = 0;
    }

    return ($available);
}

# The metatext system holds strings temporarily while building
# metadocs content

# Empties the metatext array

sub metatext_empty
{
    my ($self) = @_;

    $self->{metatext} = [];
}

# Returns the metatext buffer as a string

sub metatext_asstring
{
    my ($self) = @_;

    my @final;

    foreach my $ent (@{$self->{metatext}})
    {
        next if (!defined($ent));
        push @final,$ent;
    }
    return(join('',@final));
}

# print wrapper for metatext

sub metatext_print
{
    my ($self) = shift;

    push(@{$self->{metatext}},@_);
}

# printf wrapper for metatext

sub metatext_printf
{
    my ($self,$format,@args) = @_;

    confess("No format specified") if (!defined($format));
    push(@{$self->{metatext}},sprintf($format,@args));
}

# Get the individual elements of a meta statement
# Extract an ID if it exists
# Extract <remark>, info is either
# <remark role="FIELDNAME" condition="VALUE">
# For settings
# or
# <remark role="FIELDNAME">VALUE</remark>
# For DocBook values or titles

sub extract_meta_settings
{
    my ($metaelement,$metasettings) = @_;

    my ($targetid) = ($metaelement =~ m/id="([^\"]*?)"/);
    $$metasettings->{id} = $1if (defined($targetid));

    foreach my $remark ($metaelement =~ m{<remark\s+?role=".*?"\s+?condition=".*?"/>}gms)
    {
        $remark =~ m{<remark\s+?role="([^"]*)"\s+?condition="([^"]*)"/>}gms;
        $$metasettings->{$1} = $2;
    }
    foreach my $remark ($metaelement =~ m{<remark\s+?role="[^"]*">.*?</remark>}gms)
    {
        $remark =~ m{<remark\s+?role="([^"]*)">(.*?)</remark>}gms;
        $$metasettings->{$1} = $2;
    }
}

# Parse some text and pull out the meta elements that need parsing
#
# Items are marked with role="meta"

sub get_meta_statements
{
    my ($text) = @_;
    my @elements;

    push(@elements,
         ($text =~ m{(<(?:para|title|phrase)(?:\s+(?:id="|role="meta)[^"]*")+(?:/>|>.*?</(?:para|title|phrase)>))}msg));

    my @retelems;

    foreach my $elem (@elements)
    {
        if ($elem =~ m/role="meta"/)
        {
            push @retelems,$elem;
        }
    }

    return(@retelems);
}

sub slurp_file
{
    my ($self,$filename,$clean) = @_;

    $clean = 1 unless(defined($clean));

    my $fh = new IO::File($filename) or croak("Can't open source file ($filename): $!\n");
    binmode($fh,':utf8');
    my $inplacefile = join('',<$fh>);
    $fh->close();

# Remove commented XML
    $inplacefile =~ s{<!--.*?-->}{}msg;

# Escape entities
# These are escaped during parsing to ensure they aren't lost
    $inplacefile =~ s{(&([a-zA-Z0-9_-]*?);)}{!#!amp!#!$2!#!;!#!}msg;
                                                 return($inplacefile);
}

sub write_file
{
    my ($self,$filename,$string) = @_;

    my $fh = new IO::File($filename,'w')
        or die "Error: Can't open destination file ($filename: $!\n";
    binmode($fh,':utf8');
    syswrite($fh,$string);
    $fh->close();
}

# Take a multipart version number and then convert it into
# a numerical value so that it can be compared and sorted

sub vertodec
{
    my ($self,$version) = @_;

    confess($version) unless(defined($version));

    my ($a,$b,$c,$d) = (0,0,0,'');

    my $multiplier = 1000;

    ($a,$b,$c) = split(m/\./,$version,3);

    if (!defined($b))
    {
        $b = 0;
    }

    if (!defined($c))
    {
        $c = 0;
    }

    if ($c =~ m/(\d+)(.*)/)
    {
        $c = $1;
        $d = $2;
    }

    croak "$version not numeric" unless($a == int($a) &&
                                        $b == int($b) &&
                                        $c == int($c));

    my $decimalversion = $multiplier**4 +
        (($a * $multiplier**3) +
         ($b * $multiplier**2) +
         ($c * $multiplier) +
         ord($d));

    return ($decimalversion);
}

# Convert a decimalized version number back into a multipart
# version number

sub dectover
{
    my ($self,$version) = @_;

    my ($a,$b,$c,$d) = ($version =~ m/1(\d{3})(\d{3})(\d{3})(\d{3})/);

    return sprintf('%0d.%0d.%0d%s',$a,$b,$c,($d == 0 ? '' : chr($d)));
}

# Generate an XML tag with or without attributes

sub attrib_builder
{
    my ($attrib) = @_;

    return(join(' ',map { "$_=\"$attrib->{$_}\"" } keys %{$attrib}));
}

sub xml_tag
{
    my $self = shift;
    my $tagname = shift;

    if ((scalar @_ == 0) ||
        (!defined($_[0])))
    {
        return "<$tagname/>";
    }

    if (ref($_[0]) eq 'HASH')
    {
        return sprintf('<%s %s>%s</%s>',
                       $tagname,
                       attrib_builder($_[0]),
                       join('',@_[1..$#_]),
                       $tagname);
    }
    else
    {
        return sprintf('<%s>%s</%s>',
                       $tagname,
                       join('',@_),
                       $tagname);
    }
}

sub xml_entry
{
    my $self = shift;
    return $self->xml_tag('entry',@_);
}

sub xml_row
{
    my $self = shift;
    return $self->xml_tag('row',@_);
}

sub xml_literal
{
    my $self = shift;
    return $self->xml_tag('literal',@_);
}

sub format_version_as_docbook
{
    my ($self,$string) = @_;

    $string =~ s/\./\-/g;

    return($string);
}

# Merge docbook link
# 
# Take a list of ID/location entities and merge them using _

sub merge_docbook_link
{
    my ($self,@elements) = @_;

    my @final;
    foreach my $el (@elements)
    {
        push @final,$el if (defined($el));
    }

    return(lc(join('_',@final)));
}

# Resolve DocBook links to the right place

sub resolve_docbook_link
{
    my ($self,$id) = @_;

    return($id);
}

# Global function for generating a metadoc
# table
#
# Tables consist of the table definition, and the rows as a array;
# each row should be a row for the cells in order

sub generate_metadoc_table
{
    my ($self,$tabledef,$rows,$options) = @_;

    $self->generate_metadoc_table_header($tabledef);

    if (defined($options) && exists($options->{postsort}))
    {
        foreach my $row (sort { $a->[$options->{postsort}] cmp 
                                    $b->[$options->{postsort}] } @{$rows})
        {
            $self->generate_metadoc_tablerow($row);
        }
    }
    else
    {
        foreach my $row (@{$rows})
        {
            $self->generate_metadoc_tablerow($row);
        }
    }
    $self->metatext_printf('</tbody></tgroup></%s>',$tabledef->{type});
}

# Take a list of values as a row
# The row should be an array of cells

sub generate_metadoc_tablerow
{
    my ($self,$row) = @_;

    $self->metatext_printf('<row>');
    foreach my $cell (@{$row})
    {
        if (ref($cell) eq 'HASH')
        {
            $self->metatext_print($self->xml_entry($cell->{attr},$cell->{content}));
        }
        else
        {
            $self->metatext_print($self->xml_entry($cell));
        }
    }
    $self->metatext_printf('</row>');
}

# Create the table header
# We take the definition, adding the ID
# then output the thead group with the titles
# and then setup the content for the actual rows

sub generate_metadoc_table_header
{
    my ($self,$definition) = @_;

    $self->metatext_printf('<%s%s%s>',
                          $definition->{type},
                          (exists($definition->{id}) &&
                           defined($definition->{id}) ?
                           sprintf(' id="%s"',$definition->{id}) : ''),
                          (exists($definition->{class}) &&
                           defined($definition->{class}) ?
                           sprintf(' class="%s"',$definition->{class}) : ''),
        );

    if ($definition->{type} eq 'table' &&
        exists($definition->{title}) &&
        defined($definition->{title}) && 
        ($definition->{title} =~ m/[a-zA-Z0-9]/))
    {
        $self->metatext_printf('<title>%s</title>',$definition->{title});
    }
    elsif ($definition->{type} eq 'informaltable' &&
           exists($definition->{title}) &&
           defined($definition->{title}) && 
           ($definition->{title} =~ m/[a-zA-Z0-9]/))
    {
        $self->metatext_printf('<textobject><phrase>%s</phrase></textobject>',$definition->{title});
    }
    if ($definition->{type} eq 'table' &&
        exists($definition->{title}) &&
        defined($definition->{title}) && 
        ($definition->{title} !~ m/[a-zA-Z0-9]/))
    {
        croak(sprintf('Title not specified for a table that requires it (%s)',$definition->{id} || ''));
    }

    my @columnseq = ();

    foreach my $colid (sort { ($definition->{collist}->{$a}->{_so} || 0) <=>
                                  ($definition->{collist}->{$b}->{_so} || 0) } keys %{$definition->{collist}})
    {
        push @columnseq,$colid;
    }

    my $columncount = 0;

    foreach my $colid (@columnseq)
    {
        next if (exists($definition->{columns}->{$colid}->{namest}));
        $columncount++;
    }

    $self->metatext_printf('<tgroup cols="%d">',$columncount);

    my @titles = ();

    foreach my $colid (@columnseq)
    {
        next if (exists($definition->{collist}->{$colid}->{namest}));
        my $alignment = $definition->{collist}->{$colid}->{align} or undef;
        my $width = $definition->{collist}->{$colid}->{width} or undef;
        my $widthsuffix = '';

        $widthsuffix = '*' if (defined($width) &&
                               ($width =~ m/^[0-9]+$/));

        $self->metatext_printf('<colspec colname="%s"%s%s/>',
                          $colid,
                          (defined($alignment) ? sprintf(' align="%s"',$alignment) : ''),
                          (defined($width) ? sprintf(' colwidth="%s%s"',$width,$widthsuffix) : ''),
            );
        push(@titles,$self->xml_entry($definition->{collist}->{$colid}->{title}));
    }

    unless (exists($definition->{nocolhead}) &&
            $definition->{nocolhead} == 1)
    {
        if (exists($definition->{multiheader}))
        {
            my @rows = ();
            foreach my $rowheadspec (sort { $definition->{multiheader}->{$a}->{so} <=>
                                                $definition->{multiheader}->{$b}->{so} }
                                     keys %{$definition->{multiheader}})
            {
                my @row = ();

                foreach my $col (sort { $definition->{collist}->{$a}->{so} <=>
                                            $definition->{collist}->{$b}->{so} }
                                 @{$definition->{multiheader}->{$rowheadspec}->{fields}})
                {
                    my $colopts = {};
                    foreach my $opt (qw/morerows namest nameend/)
                    {
                        $colopts->{$opt} = $definition->{collist}->{$col}->{$opt}
                        if (exists($definition->{collist}->{$col}->{$opt}));
                    }
                    push @row,$self->xml_entry($colopts,$definition->{collist}->{$col}->{title});
                }
                push @rows,$self->xml_row(@row);
            }
            $self->metatext_print('<thead>',
                                  @rows,
                                  '</thead>');
        }
        else
        {
            $self->metatext_print('<thead>',
                                  $self->xml_row(@titles),
                                  '</thead>');
        }
    }

    $self->metatext_printf('<tbody>');
}

sub validate_object_version
{
    my ($self,$identity,$object) = @_;

    if (exists($object->{iv}) &&
        exists($object->{ov}) &&
        ($self->vertodec($object->{ov}) <
         $self->vertodec($object->{iv}))
        )
    {
        $self->meta_warning(sprintf('Object %s: Out version is less than In version',$identity));
    }
    if (exists($object->{dv}) &&
        exists($object->{ov}) &&
        ($self->vertodec($object->{dv}) >
         $self->vertodec($object->{ov}))
        )
    {
        $self->meta_warning(sprintf('Object %s: Deprecated version is greater than Out version',$identity));
    }
    if (exists($object->{rv}) &&
        exists($object->{ov}) &&
        ($self->vertodec($object->{rv}) >
        $self->vertodec($object->{ov}))
        )
    {
        $self->meta_warning(sprintf('Object %s: Deprecated version is greater than Removed version',$identity));
    }
}

sub describe_object_version
{
    my ($self,$object) = @_;

    my $struct = {};

    if (exists($object->{iv}))
    {
        $struct->{introduced} = sprintf('Introduced v%s',$object->{iv});
        $struct->{introduced_short} = sprintf('Int. v%s',$object->{iv});
        $struct->{introduced_vonly} = $object->{iv};
    }
    if (exists($object->{ov}))
    {
        $struct->{removed} = sprintf('Removed v%s',$object->{ov});
        $struct->{removed_short} = sprintf('Removed v%s',$object->{ov});
        $struct->{removed_vonly} = $object->{ov};
    }
    if (exists($object->{dv}))
    {
        $struct->{deprecated} = sprintf('Deprecated v%s',$object->{dv});
        $struct->{deprecated_short} = sprintf('Dep. v%s',$object->{dv});
        $struct->{deprecated_vonly} = $object->{dv};
    }

    return($struct);
}

sub describe_object_version_astext
{
    my ($self,$object,$type) = @_;

    $type = '' unless(defined($type));
    $type = ''if (($type ne 'short') ||
                  ($type ne 'vonly'));
    
    my $vs = $self->describe_object_version($object);

    my @s;
    foreach my $vtype (qw/introduced deprecated removed/)
    {
        push(@s,$vs->{sprintf('%s%s',$vtype,$type)})
            if (exists($vs->{$vtype}));
    }

    if (scalar(@s) == 0)
    {
        return undef;
    }
    else
    {
        return(join(', ',@s));
    }
}

sub meta_error
{
    my ($self,$text) = @_;

    printf STDERR ("Error: %s: %s\n",
                  $self->{_modulename} || 'CouchDocs',
                  $text
          );
    exit(1);
}

sub meta_warning
{
    my ($self,$text) = @_;

    printf STDERR ("Warning: %s: %s\n",
                   $self->{_modulename} || 'CouchDocs',
                   $text
        );
}

sub deep_copy
{
    my $self = shift;
    my $this = shift;
    if (not ref $this) {
        $this;
    } elsif (ref $this eq 'ARRAY') {
        [map $self->deep_copy($_), @$this];
    } elsif (ref $this eq 'HASH') {
        +{map { $_ => $self->deep_copy($this->{$_}) } keys %$this};
    } else { die 'what type is $_?' }
}

sub setbyattrib
{
    my ($self,$item,$element,$list) = @_;

    foreach my $list_item (@{$list})
    {
        if (exists($element->{Attributes}->{$list_item}))
        {
            $$item->{$list_item} = $element->{Attributes}->{$list_item};
        }
    }
}

sub escape_string
{
    my ($rawtext) = @_;

    $rawtext =~ s/&/&amp;/g;
    $rawtext =~ s/&amp;([a-z_]);/&$1;/g;
    $rawtext =~ s/</&lt;/g;
    $rawtext =~ s/>/&gt;/g;

    return($rawtext);
}

sub characters
{
    my ($self, $element) = @_;
#    $element->{Data} =~ s/[\r\n]+/ /g;
    while ($element->{Data} =~ s/[ ][ ]+/ /g) {}

    if ($self->{captext})
    {
        $self->{currenttext} .= escape_string($element->{Data});
    }
}

sub entity_reference
{
    my ($self,$element) = @_;

    $self->{currenttext} . sprintf('&%s;',$element->{Name}) if ($self->{savecdata});
}

sub remap_xml_enable
{
    my ($self,$element) = @_;

    $self->{remapxmldata} = 1;
    $self->{captext} = 1;
}

sub remap_xml_disable
{
    my ($self,$element) = @_;

    $self->{remapxmldata} = 0;
    $self->{captext} = 0;
}

sub remap_xml_start
{
    my ($self,$element) = @_;

    if ($self->{remapxmldata})
    {
        $self->{currenttext} .= sprintf('<%s',$element->{Name});

        foreach my $attrib (keys %{$element->{Attributes}})
        {
            $self->{currenttext} .= sprintf(' %s="%s"',$attrib,$element->{Attributes}->{$attrib});
        }
        $self->{currenttext} .= '>';

        return 1;
    }
    return 0;
}

sub remap_xml_end
{
    my ($self,$element) = @_;

    if ($self->{remapxmldata})
    {
        $self->{currenttext} .= sprintf('</%s>',$element->{Name});
        return 1;
    }
    return 0;
}

sub reset_parser_charbuffer
{
    my ($self) = @_;

    my $text = $self->{currenttext};
    $self->{currenttext} = '';
    $self->{captext} = 0;
    return($text);
}

sub get_xml_region
{
    my ($self) = @_;
    return join(' -> ',grep(/_xmlregion_/,keys %{$self}));
}

sub check_xml_region
{
    my ($self,$element) = @_;
    return($self->{xml_region_format($element)});
}

sub check_xml_mregion
{
    my ($self,@elems) = @_;

    my $count = 0;
    my $noncount = 0;

    my $checklist = {};

    foreach my $element (@elems)
    {
        my $felement = xml_region_format($element);
        $checklist->{$felement} = 1;
    }

    foreach my $element (@{$self->{xmlregions}})
    {
        my $felement = xml_region_format($element);

        if (exists($checklist->{$felement}) &&
            $self->{$felement} == 1)
        {
            $count++;
        }
        else
        {
            return(0);
        }
    }
    return(1);
}

sub set_xml_region
{
    my ($self,$element) = @_;

    foreach my $check (@{$self->{xmlregions}})
    {
        if ($check eq $element)
        {
            my $felement = xml_region_format($element);
            $self->{$felement} = 1;
        }
    }
}

sub xml_region_format
{
    my ($region) = @_;

    return(sprintf('_xmlregion_%s',$region));
}

sub unset_xml_region
{
    my ($self,$element) = @_;

    foreach my $check (@{$self->{xmlregions}})
    {
        $self->{xml_region_format($element)} = 0
            if ($check eq $element);
    }
}

1;
