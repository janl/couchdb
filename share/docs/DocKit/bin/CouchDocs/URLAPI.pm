package CouchDocs::URLAPI;

use strict;
use warnings;
use Carp;
use XML::Parser::PerlSAX;

use vars qw/@ISA/;
@ISA = ('CouchDocs');

use CouchDocs::URLAPI::Parser;

sub new
{
    my ($self) = @_;

    my $class = ref($self) || $self;

    return bless {
        _modulename => 'CouchDocs::URLAPI',
        meta_opts => {
            basedir => 'urlapi',
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
    if ($metasettings->{output} eq 'accesstable')
    {
        $self->accesstable($metasettings);
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

sub summarytable
{
    my ($self,$metasettings) = @_;

    my $tabledef = {
        title => $metasettings->{title},
        id => $metasettings->{id} || undef,
        type => 'table',
        collist => {
            method => {
                _so => 1,
                title => 'Method',
            },
            path => {
                _so => 2,
                title => 'Path',
            },
            desc => {
                _so => 3,
                title => 'Description',
            },
        },
    };

    my $td = $self->{metacache}->{$metasettings->{src}}->{paths};

    my @rows;

    foreach my $id (sort { $td->{$a}->{path} cmp
                              $td->{$b}->{path} }
                    keys %{$td})
    {
        next unless($self->meta_object_filter($metasettings,
                                              $td->{$id}));

        foreach my $access (sort { $a->{_so} <=> $b->{_so} } 
                            @{$td->{$id}->{access}})
        {
            next unless($self->meta_object_filter($metasettings,
                                                  $access));
            my @row;

            push @row,sprintf('<literal>%s</literal>',$access->{method});
            push @row,sprintf('<literal>%s</literal>',$td->{$id}->{path});
            if (                   
                exists($td->{$id}->{xrefto})
                )
            {
                push(@row,sprintf('<link linkend="%s">%s</link>',
                                  $self->merge_docbook_link($metasettings->{idprefix},
                                                            $td->{$id}->{xrefto},
                                                            lc($access->{method}),
                                                     ),
                                  $access->{description}));
            }
            push @rows,\@row;
        }
    }

    $self->generate_metadoc_table($tabledef,\@rows);
}

sub accesstable
{
    my ($self,$metasettings) = @_;

    my $entry = undef;

    my $urlbase = $self->{metacache}->{$metasettings->{src}}->{paths}->{$metasettings->{itemid}};

    if (!defined($urlbase))
    {
        $self->meta_error("URL item $metasettings->{itemid} not found");
    }

    foreach my $find (@{$urlbase->{access}})
    {
        $entry = $find
            if ($find->{method} eq $metasettings->{method});
    }

    use Data::Dumper;
    if (!defined($entry))
    {
       confess "$metasettings->{method} in $metasettings->{itemid} not found";
    }

    my $tabledef = {
        title => $metasettings->{title} || sprintf('URL API %s %s',$entry->{method},$urlbase->{path}),
        id => $metasettings->{id} || undef,
        nocolhead => 1,
        type => 'informaltable',
        collist => {
            field => {
                _so => 1,
            },
            info => {
                _so => 2,
            },
            addinfo => {
                _so => 3,
            },
        },
    };

    my @rows;

    if (!exists($metasettings->{opt_queryargs_only}) ||
        $metasettings->{opt_queryargs_only} eq "no")
    {
        push(@rows,
             $self->accesstable_row('Method',
                                    {
                                        attr => {
                                            namest => 'info',
                                            nameend => 'addinfo',
                                        },
                                        content => sprintf('<literal>%s %s</literal>',
                                                           $entry->{method},
                                                           $urlbase->{path},
                                            ),
                                    }
             ),
             $self->accesstable_row('Request',
                                    {
                                        attr => {
                                            namest => 'info',
                                            nameend => 'addinfo',
                                        },
                                        content => $entry->{request},
                                    }
             ),
             $self->accesstable_row('Response',
                                    {
                                        attr => {
                                            namest => 'info',
                                            nameend => 'addinfo',
                                        },
                                        content => $entry->{response},
                                    }
             ),
             $self->accesstable_row('Admin Privileges Required',
                                    {
                                        attr => {
                                            namest => 'info',
                                            nameend => 'addinfo',
                                        },
                                        content => $entry->{admin} || 'no',
                                    }
             ),
            );
    }

    if (exists($entry->{qa}) &&
        scalar @{$entry->{qa}})
    {
# First we work out if there is a list of valid QAs for the specified version        
        my @qas;

        foreach my $qa (sort { ($a->{iv} || $a->{ov} || 1.0) <=> 
                                   ($b->{iv} || $b->{ov} || 9.99.99) }
                        @{$entry->{qa}})
        {
            next unless($self->meta_object_filter($metasettings,$qa));
            push @qas,$qa;
        }

# TODO Need to display version information 

        my $rows = 0;
        foreach my $qa (@qas)
        {
            my $header = 'Query Arguments';

# Proceed through the list of arguments

            foreach my $arg (sort { $a->{name} cmp $b->{name} } @{$qa->{args}})
            {
                $header = undef if ($rows > 0);

                if (!exists($metasettings->{opt_queryargs_only}) ||
                    $metasettings->{opt_queryargs_only} eq "no")
                {
                    push(@rows,[
                             (defined($header) ? $self->xml_tag('emphasis',{role => 'bold'},$header) : ''),
                             $self->xml_tag('emphasis',{role => 'bold'},'Argument'),
                             $self->xml_tag('literal',$arg->{name}),
                         ]);
                }
                else
                {
                    push(@rows,[
                             $self->xml_tag('emphasis',{role => 'bold'},'Argument'),
                             $self->xml_tag('literal',$arg->{name}),
                         ]);
                }                    

                $rows++;

                foreach my $field (qw/description opt type default min max qty/)
                {
                    my $row;

                    $row = $self->accesstable_subrow($arg,$field);

                    push @rows,$row if (defined($row));
                }

                if (exists($arg->{values}) && 
                    scalar @{$arg->{values}})
                {
                    push(@rows,[
                             '',
                             $self->xml_tag('emphasis',{role => 'bold'},'Supported Values'),
                         ]);
                    foreach my $value (sort { $a->{value} cmp $b->{value} } @{$arg->{values}})
                    {
                        next unless($self->meta_object_filter($metasettings,$value));
                        my $qaverspec = $self->describe_object_version_astext($value);
                        push(@rows,[
                                 '',
                                 $self->xml_tag('literal',$value->{value}),
                                 sprintf('%s%s',
                                         $value->{description},
                                         (defined($qaverspec) ? sprintf(' (%s)',$qaverspec) : ''),
                                 ),
                             ]);
                    }
                }                        
# Insert a blank row 
                push @rows,['','',''];
            }
        }
    }

    if (($rows[-1]->[0] eq '') &&
        ($rows[-1]->[1] eq '') &&
        ($rows[-1]->[2] eq '')
    )
    {
        pop @rows;
    }

    if (exists($entry->{httpheaders}) &&
        scalar @{$entry->{httpheaders}})
    {
# First we work out if there is a list of valid HTTP Headers for the specified version        
        my @hhs;

        foreach my $hh (sort { ($a->{iv} || $a->{ov} || 1.0) <=> 
                                   ($b->{iv} || $b->{ov} || 9.99.99) }
                        @{$entry->{httpheaders}})
        {
            next unless($self->meta_object_filter($metasettings,$hh));
            push @hhs,$hh;
        }

# TODO Need to display version information 

        my $rows = 0;
        foreach my $httpheader (@hhs)
        {
            my $header = 'HTTP Headers';

# Proceed through the list of arguments

            foreach my $arg (sort { $a->{name} cmp $b->{name} } @{$httpheader->{headers}})
            {
                $header = undef if ($rows > 0);
                push(@rows,[
                         (defined($header) ? $self->xml_tag('emphasis',{role => 'bold'},$header) : ''),
                         $self->xml_tag('emphasis',{role => 'bold'},'Header'),
                         $self->xml_tag('literal',$arg->{name}),
                     ]);
                $rows++;

                foreach my $field (qw/description opt type default min max qty/)
                {
                    my $row = 
                        $self->accesstable_subrow($arg,$field);
                    push @rows,$row if (defined($row));
                }

                if (scalar keys %{$arg->{values}})
                {
                    push(@rows,[
                             '',
                             $self->xml_tag('emphasis',{role => 'bold'},'Supported Values'),
                         ]);
                    foreach my $value (sort { $a cmp $b } keys %{$arg->{values}})
                    {
                        push(@rows,[
                                 '',
                                 $self->xml_tag('literal',$value),
                                 $arg->{values}->{$value},
                             ]);
                    }
                }                        
# Insert a blank row 
                push @rows,['','',''];
            }
        }
    }

    if (scalar keys %{$entry->{returncodes}} > 0)
    {
        my $rows = 0;

        foreach my $returncode (sort keys %{$entry->{returncodes}})
        {
            if ($rows == 0)
            {
                push(@rows,[{attr => { namest => 'field',
                                       nameend => 'addinfo',},
                             content => $self->xml_tag('emphasis',{role => 'bold'},'Return Codes')},]);
            }

            push(@rows,[$returncode,
                        {attr => { namest => 'info',
                                   nameend => 'addinfo',},
                         content => $entry->{returncodes}->{$returncode}},]);
            
            $rows++;
        }
    }

    $self->generate_metadoc_table($tabledef,\@rows);
}

sub accesstable_subrow
{
    my ($self,$arg,$field) = @_;

    my $fieldtitle = { 
        'description' => 'Description',
        'opt' => 'Optional',
        'type' => 'Type',
        'default' => 'Default',
        'min' => 'Minimum',
        'max' => 'Maximum',
        'qty' => 'Quantity',
    };

    if (exists($arg->{$field}))
    {
        return([
            '',
            $self->xml_tag('emphasis',{role => 'bold'},$fieldtitle->{$field}),
            $arg->{$field},
               ]) 
    }
    return(undef);
}

sub accesstable_row
{
    my ($self,$title,@content) = @_;

    my @row;

    push(@row,
         sprintf('<emphasis role="bold">%s</emphasis>',
                 $title),
         @content,
         );
    return(\@row);
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

    my $metaparser = CouchDocs::URLAPI::Parser->new();
    eval {
      XML::Parser::PerlSAX->new->parse(Source => {String => $metafile,},
                                       Handler => $metaparser,

          );
    };

    $self->{filecache}->{$fileref} = 1;
    $self->{dependencies}->{$fileref} = 1;
    $self->{dependencies}->{$self->resolve_module_path('CouchDocs::URLAPI::Parser')} = 1;
    $self->{metacache}->{$metasettings->{src}} = $metaparser->{data};
}

1;
