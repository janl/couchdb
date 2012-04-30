package CouchDocs::JSON;

use strict;
use warnings;
use Carp;
use XML::Parser::PerlSAX;

use Data::Dumper;

use vars qw/@ISA/;
@ISA = ('CouchDocs');

use CouchDocs::JSON::Parser;

sub new
{
    my ($self) = @_;

    my $class = ref($self) || $self;

    return bless {
        _modulename => 'CouchDocs::JSON',
        meta_opts => {
            basedir => 'json',
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

    if ($metasettings->{output} eq 'summarytable')
    {
        $self->summarytable($metasettings);
    }
    if ($metasettings->{output} eq 'alltables')
    {
        $self->alltables($metasettings);
    }
    if ($metasettings->{output} eq 'itemtable')
    {
        $self->itemtable($metasettings);
    }
    if ($metasettings->{output} eq 'itemsample')
    {
        $self->itemsample($metasettings);
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

sub alltables
{
    my ($self,$metasettings) = @_;

    my @tables;

    my $td = $self->{metacache}->{$metasettings->{src}};
    $self->metatext_empty();

    foreach my $structureid (sort { ($td->{$a}->{description} or $a) cmp
                                        ($td->{$b}->{description} or $b) } keys %{$td})
    {
        next if ($td->{$structureid}->{sub} == 1);
        $self->metatext_empty();

        # Only top-level structures are listed

        my $tablesettings = $self->deep_copy($metasettings);
        $tablesettings->{id} = $self->merge_docbook_link($metasettings->{idprefix},
                                                         $structureid);
        $tablesettings->{itemid} = $structureid;
        $self->itemtable($tablesettings);
        push @tables,$self->metatext_asstring();
    }
    $self->metatext_empty();
    $self->metatext_print(join('',@tables));
}

sub summarytable
{
    my ($self,$metasettings) = @_;

    my $tabledef = {
        title => $metasettings->{title},
        id => $metasettings->{id} || undef,
        type => 'table',
        collist => {
            desc => {
                _so => 3,
                title => 'Description',
            },
        },
    };

    my @rows;

    my $td = $self->{metacache}->{$metasettings->{src}};

    foreach my $structureid (sort { ($td->{$a}->{description} or $a) cmp
                                        ($td->{$b}->{description} or $b) } keys %{$td})
    {
        # Only top-level structures are listed
        next if ($td->{$structureid}->{sub} == 1);

        my @row;

        push @row,$self->xml_tag('link',
                                 {
                                     linkend => 
                                         $self->merge_docbook_link($metasettings->{idprefix},
                                                                   $structureid),
                                 },
                                 $td->{$structureid}->{description});
        
        push @rows,\@row;
    }
    $self->generate_metadoc_table($tabledef,\@rows); 
}

sub itemtable
{
    my ($self,$metasettings) = @_;

    my $baseitem = $self->{metacache}->{$metasettings->{src}}->{$metasettings->{itemid}};

    if (!defined($baseitem))
    {
        $self->meta_error("Cannot find definition for $metasettings->{itemid}\n");
    }

# Work through the entire structure and calculate the width
# We can go back and add in the table column headings later

    my @rows;

    push @rows, $self->itemtable_row(0,
                                     0,
                                     $baseitem,
                                     $metasettings,
        );

#    my $width = 0;
#    foreach my $row (@rows)
#    {
#        $width = scalar(@{$row})
#            if (scalar(@{$row}) > $width);
#    }

    my $tabledef = {
        title => $metasettings->{title} || $baseitem->{description},
        id => $metasettings->{id} || undef,
        nocolhead => 1,
        type => 'table',
        class => 'jsonstructure',
        collist => {
            item => { 
                _so => 1,
                title => 'Field',
                width => 30,
            },
            desc => {
                _so => 99,
                title => 'Description',
                width => 70,
            },
        },
    };

# Set the individual columns for the field nest

#    my $colwidth = int(30/$width);

#    foreach my $id (0..($width-1))
#    {
#        $tabledef->{collist}->{sprintf('item-%d',$id)} = {
#            _so => $id,
#            title => sprintf('item-%d',$id),
#            width => $colwidth,
#        };
#    }

#     $tabledef->{collist}->{lastitem} = {
#             _so => 98,
#             title => 'lastitem',
#             width => $colwidth,
#     };

    unshift(@rows,
            [
             {
                 attr => { 
#                     namest => sprintf('item-%d',0),
#                     nameend => 'lastitem',
                 },
                 content => $self->xml_tag('emphasis',{role => 'bold'},'Field')
             },
             $self->xml_tag('emphasis',{role => 'bold'},'Description')
            ]
        );

    $self->generate_metadoc_table($tabledef,\@rows); 
}

sub itemtable_row
{
    my ($self,$depth,$parentdepth,$item,$metasettings) = @_;

    my @rows;

    if (exists($item->{merge}))
    {
        foreach my $include (sort keys %{$item->{merge}})
        {
            push @rows,$self->itemtable_row($depth,
                                            $depth,
                                            $self->{metacache}->{$metasettings->{src}}->{$include},
                                            $metasettings);
        }
    }

    foreach my $field (sort keys %{$item->{fields}})
    {
        my @row;

        my $type = '';

        if (exists($item->{fields}->{$field}->{type}) &&
            ($item->{fields}->{$field}->{type} eq 'array'))
        {
            $type = $self->xml_tag('literal','[array]');
        }

        my $optional = '';
        
        if (exists($item->{fields}->{$field}->{optional}) &&
            ($item->{fields}->{$field}->{optional} eq 'yes'))
        {
            $type = '(optional)';
        }

# Some sections removed temporarily

#         push(@row,
#              {
#                  attr => {
#                      namest => sprintf('item-%d',0),
#                      nameend => sprintf('item-%d',$parentdepth),
#                  },
#                  content => '',
#              })
#             if ($depth > 0);

        push(@row,
             {
                 attr => {
#                     namest => sprintf('item-%d',$depth),
#                     nameend => sprintf('lastitem',$parentdepth),
                 },
                 content => sprintf('%s%s %s %s',
                                    ('&nbsp;' x ($depth*8)),
                                    $self->xml_tag('literal',$field),
                                    $type,
                                    $optional),
             });

        if (exists($item->{fields}->{$field}->{merge}))
        {
            foreach my $include (sort keys %{$item->{fields}->{$field}->{merge}})
            {
                push(@row,
                             $self->{metacache}->{$metasettings->{src}}->{$include}->{description},
                    );
                push @rows,\@row;
                push @rows,$self->itemtable_row(($depth+1),
                                                $depth,
                                                $self->{metacache}->{$metasettings->{src}}->{$include},
                                                $metasettings);
            }
        }
        else
        {
            push @row,$item->{fields}->{$field}->{description};
            push @rows,\@row;
        }
    }

    return(@rows);
}


sub itemsample
{
    my ($self,$metasettings) = @_;

    my $baseitem = $self->{metacache}->{$metasettings->{src}}->{$metasettings->{itemid}};

# All JSON objects are exactly that, objects

    

}

sub itemsample_builder
{
    my ($self,$itemid,$metasettings) = @_;

    

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

    my $metaparser = CouchDocs::JSON::Parser->new();
    eval {
      XML::Parser::PerlSAX->new->parse(Source => {String => $metafile,},
                                       Handler => $metaparser,

          );
    };

    $self->{filecache}->{$fileref} = 1;
    $self->{dependencies}->{$fileref} = 1;
    $self->{dependencies}->{$self->resolve_module_path('CouchDocs::JSON::Parser')} = 1;
    $self->{metacache}->{$metasettings->{src}} = $metaparser->{data};
}

1;
