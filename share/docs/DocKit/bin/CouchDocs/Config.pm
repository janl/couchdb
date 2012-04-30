package CouchDocs::Config;

use strict;
use warnings;
use Carp;
use XML::Parser::PerlSAX;

use Data::Dumper;

use vars qw/@ISA/;
@ISA = ('CouchDocs');

use CouchDocs::Config::Parser;

sub new
{
    my ($self) = @_;

    my $class = ref($self) || $self;

    return bless {
        _modulename => 'CouchDocs::Config',
        meta_opts => {
            basedir => 'config',
        },
        metacache => {},
        filecache => {},
    };
}

sub parse
{
    my ($self,$metasettings) = @_;

    my $retval = {text => '',
                  _files => ''};

    $self->load_metasrc($metasettings);

    $self->metatext_empty();

    if ($metasettings->{output} eq 'classsummarytable')
    {
        $self->classsummarytable($metasettings);
    }
    if ($metasettings->{output} eq 'optionsummarytable')
    {
        $self->optionsummarytable($metasettings);
    }
    if ($metasettings->{output} eq 'itemtable')
    {
        $self->itemtable($metasettings);
    }

    $retval->{dependencies} = $self->{dependencies};
    $retval->{text} = $self->metatext_asstring();

    return $retval;
}

sub validate
{
    my ($self,$metafile) = @_;

    $self->load_metasrc({src => $metafile});
}

sub classsummarytable
{
    my ($self,$metasettings) = @_;

    my $tabledef = {
        title => $metasettings->{title},
        id => $metasettings->{id} || undef,
        type => 'table',
        collist => {
            section => {
                _so => 1,
                title => 'Section',
            },
            desc => {
                _so => 3,
                title => 'Description',
            },
        },
    };

    my @rows;

    my $td = $self->{metacache}->{$metasettings->{src}}->{classes};

    foreach my $structureid (sort { ($td->{$a}->{description} or $a) cmp
                                        ($td->{$b}->{description} or $b) } keys %{$td})
    {
        next unless ($self->meta_object_filter($metasettings,$td->{$structureid}));
        my @row;

        push(@row,
             (exists($metasettings->{idprefix}) && defined($metasettings->{idprefix}) ? 
              $self->xml_tag('link',
                             {
                                 linkend => 
                                     $self->merge_docbook_link($metasettings->{idprefix},
                                                               $structureid),
                             },
                             $self->xml_literal($structureid)) : $self->xml_literal($structureid)),
             $td->{$structureid}->{description});
        
        push @rows,\@row;
    }
    $self->generate_metadoc_table($tabledef,\@rows); 
}

sub optionsummarytable
{
    my ($self,$metasettings) = @_;

    my $tabledef = {
        title => $metasettings->{title},
        id => $metasettings->{id} || undef,
        type => 'table',
        collist => {
            option => {
                _so => 1,
                title => 'Option',
            },
            desc => {
                _so => 3,
                title => 'Description',
            },
        },
    };

    my @rows;

    my $optionlist = $self->{metacache}->{$metasettings->{src}}->{optionsbyclass}->{$metasettings->{itemid}};

    my $td = $self->{metacache}->{$metasettings->{src}}->{options};

#    print STDERR "\n\nPREF for $metasettings->{itemid}\n\n";

    foreach my $structureid (sort { ($td->{$a}->{description} or $a) cmp
                                        ($td->{$b}->{description} or $b) } keys %{$optionlist})
    {
        my @row;

        push(@row,
             ((exists($metasettings->{idprefix}) &&
               defined($metasettings->{idprefix})) ? 
              $self->xml_tag('link',
                             {
                                 linkend => 
                                     $self->merge_docbook_link($metasettings->{idprefix},
                                                               $structureid),
                             },
                             $self->xml_literal($structureid)) : $self->xml_literal($structureid)
              ),
             $td->{$structureid}->{description});

        push @rows,\@row;
        next;
 
       print <<EOF;
TODO: Fill out this section
  <section id="config-couchdb-options_attachments_$structureid">

    <title><literal>$structureid</literal> Option</title>

    <para>
      Some...
    </para>

    <para role="meta" id="table-couchdb-config-options-attachments-$structureid-detail">
      <remark role="title"><literal>$structureid</literal></remark>
      <remark role="type" condition="config"/>
      <remark role="src" condition="couchdb"/>
      <remark role="output" condition="itemtable"/>
      <remark role="version" condition="1.0"/>
      <remark role="itemid" condition="$structureid"/>
    </para>

  </section>
EOF
    }
    $self->generate_metadoc_table($tabledef,\@rows);
#    print STDERR "\n\n";
}

sub itemtable
{
    my ($self,$metasettings) = @_;

    my $baseitem = $self->{metacache}->{$metasettings->{src}}->{$metasettings->{itemid}};
}

sub load_metasrc
{
    my ($self,$metasettings) = @_;

    my $fileref = sprintf('%s/%s/%s.xml',
                          $self->{global_opts}->{metaroot},
                          $self->{meta_opts}->{basedir},
                          $metasettings->{src},
        );

    return if (exists($self->{filecache}->{$fileref}));

    croak("Can't find $fileref")
        unless(-f $fileref);

    my $metafile = $self->slurp_file($fileref);

    my $metaparser = CouchDocs::Config::Parser->new();
    eval {
      XML::Parser::PerlSAX->new->parse(Source => {String => $metafile,},
                                       Handler => $metaparser,

          );
    };

    $self->{filecache}->{$fileref} = 1;
    $self->{dependencies}->{$fileref} = 1;
    $self->{dependencies}->{$self->resolve_module_path('CouchDocs::Config::Parser')} = 1;
    $self->{metacache}->{$metasettings->{src}} = $metaparser->{data};
}

1;
