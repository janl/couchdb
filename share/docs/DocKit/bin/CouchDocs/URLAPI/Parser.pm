package CouchDocs::URLAPI::Parser;

use strict;
use warnings;
use Data::Dumper;
use Carp;
use File::Basename;

use vars qw/@ISA/;
@ISA = ('CouchDocs');

sub new
{
    my ($self) = @_;

    my $class = ref($self) || $self;

    return bless {
        _modulename => 'URLAPI::Parser',
# holds the data
        data => { 
            paths => {},
            _defaultc => {},
        },
# tracks whether we are reading default return codes
        indefaultrc => 0,
# indicates whether to capture text or not
        captext => 0,
        currenttext => '',
# Used to sort method names in the output
        methodso => {
            'GET' => 1,
            'HEAD' => 2,
            'PUT' => 4,
            'POST' => 8,
            'DELETE' => 16,
            'COPY' => 32,
        },
    },$class;
}

sub start_element
{
    my ($self,$element) = @_;

    if ($element->{Name} eq 'defaultreturncodes')
    {
        $self->{indefaultrc} = 1;
    }

    if ($element->{Name} eq 'urlapi')
    {
        if (!exists($element->{Attributes}->{id}))
        {
            croak("id is a required attributed to urlapi");
        }
        if (!exists($element->{Attributes}->{path}))
        {
            croak("path is a required attributed to urlapi");
        }
        $self->{currentid} = $element->{Attributes}->{id};
        if (exists($self->{data}->{paths}->{$self->{currentid}}))
        {
            croak("ID $self->{currentid} already exists");
        }
        $self->{data}->{paths}->{$self->{currentid}} = {
            id => $self->{currentid},
        };

        $self->setbyattrib(\$self->{data}->{paths}->{$self->{currentid}},$element,[qw/class path xrefto iv ov dv rv/]);
        $self->validate_object_version("URLAPI path $self->{data}->{paths}->{$self->{currentid}}->{path}",
                                       $self->{data}->{paths}->{$self->{currentid}});
    }
    elsif ($element->{Name} eq 'access')
    {
        $self->{currentaccess} = {
            qa => [],
        };

        $self->setbyattrib(\$self->{currentaccess},$element,[qw/method xrefto iv ov dv rv admin/]);
        if (!exists($self->{currentaccess}->{method}))
        {
            croak("Attribute method is required for an access entry");
        }
        $self->validate_object_version("URLAPI path $self->{data}->{paths}->{$self->{currentid}}->{path} method $self->{currentaccess}->{method}",
                                       $self->{currentaccess});
    }
    elsif ($element->{Name} eq 'queryargs')
    {
        $self->{currentqa} = {
            args => [],
        };
        $self->setbyattrib(\$self->{currentqa},$element,[qw/iv ov dv rv/]);
        $self->validate_object_version("Query args in path $self->{data}->{paths}->{$self->{currentid}}->{path} method $self->{currentaccess}->{method}",
                                       $self->{currentaccess});
    }
    elsif ($element->{Name} eq 'httpheaders')
    {
        $self->{currenthttpheaders} = {
            headers => [],
        };
        $self->setbyattrib(\$self->{currenthttpheaders},$element,[qw/iv ov dv rv/]);
    }
    elsif (($element->{Name} eq 'description') ||
           ($element->{Name} eq 'request') ||
           ($element->{Name} eq 'response')
           )
    {
        $self->{captext} = 1;
    }
    elsif ($element->{Name} eq 'returncode')
    {
        croak("Must specify the return code") unless(exists($element->{Attributes}->{code}) &&
                                                      defined($element->{Attributes}->{code}));
        $self->{currenterrorcode} = $element->{Attributes}->{code};
        $self->{captext} = 1;
    }
    elsif ($element->{Name} eq 'arg')
    {
        $self->{currentarg} = {};
        $self->meta_error("Must specify an argument name") unless(exists($element->{Attributes}->{name}) &&
                                                                    defined($element->{Attributes}->{name}));
        
        $self->setbyattrib(\$self->{currentarg},$element,[qw/name type default min max opt qty/]);
    }
    elsif ($element->{Name} eq 'httpheader')
    {
        $self->{currenthttpheader} = {};
        $self->meta_error("Must specify an http header name") unless(exists($element->{Attributes}->{name}) &&
                                                                       defined($element->{Attributes}->{name}));

        $self->setbyattrib(\$self->{currenthttpheader},$element,[qw/name type default min max opt qty/]);
    }
    elsif ($element->{Name} eq 'argdesc')
    {
        $self->{captext} = 1;
    }
    elsif ($element->{Name} eq 'httpheaderdesc')
    {
        $self->{captext} = 1;
    }
    elsif ($element->{Name} eq 'option')
    {
        croak("Must specify an value name") unless(exists($element->{Attributes}->{value}) &&
                                                      defined($element->{Attributes}->{value}));
        $self->{currentoption} = {
            value => $element->{Attributes}->{value},
        };
        
        $self->setbyattrib(\$self->{currentoption},$element,[qw/iv ov dv rv/]);
        $self->{captext} = 1;
    }
}

# Most of the validation should occur here, before the info
# is stored and written to the final structure

sub end_element
{
    my ($self,$element) = @_;

    if ($element->{Name} eq 'defaultreturncodes')
    {
        $self->{indefaultrc} = 0;
    }

    if ($element->{Name} eq 'arg')
    {
        push(@{$self->{currentqa}->{args}}, $self->{currentarg});
    }
    if ($element->{Name} eq 'httpheader')
    {
        push(@{$self->{currenthttpheaders}->{headers}}, $self->{currenthttpheader});
    }
    elsif ($element->{Name} eq 'queryargs')
    {
        if (scalar @{$self->{currentqa}->{args}} < 1)
        {
            $self->meta_error("Cannot have a Query Args section with no arguments");
        }
        push(@{$self->{currentaccess}->{qa}},$self->{currentqa});
    }
    elsif ($element->{Name} eq 'httpheaders')
    {
        push(@{$self->{currentaccess}->{httpheaders}},$self->{currenthttpheaders});
    }
    elsif ($element->{Name} eq 'description')
    {
        $self->{currentaccess}->{description} = $self->{currenttext};
        $self->reset_parser_charbuffer();    
    }
    elsif ($element->{Name} eq 'argdesc')
    {
        $self->{currentarg}->{description} = $self->{currenttext};
        $self->reset_parser_charbuffer();    
    }
    elsif ($element->{Name} eq 'httpheaderdesc')
    {
        $self->{currenthttpheader}->{description} = $self->{currenttext};
        $self->reset_parser_charbuffer();    
    }
    elsif ($element->{Name} eq 'option')
    {
        $self->meta_error('Option value must be filled') unless($self->{currenttext} =~ m/[a-z0-9]/i);
        $self->{currentoption}->{description} = $self->{currenttext};
        push @{$self->{currentarg}->{values}},$self->{currentoption};
        $self->reset_parser_charbuffer();    
    }
    elsif ($element->{Name} eq 'request')
    {
        $self->{currentaccess}->{request} = $self->{currenttext};
        $self->reset_parser_charbuffer();    
    }
    elsif ($element->{Name} eq 'response')
    {
        $self->{currentaccess}->{response} = $self->{currenttext};
        $self->reset_parser_charbuffer();    
    }
    elsif ($element->{Name} eq 'returncode')
    {
        if ($self->{indefaultrc} == 1)
        {
            $self->meta_error('Returncode description must be filled') unless($self->{currenttext} =~ m/[a-z0-9]/i);
            $self->{data}->{_defaultrc}->{$self->{currenterrorcode}} = $self->{currenttext};
        }
        else
        {
# If the return code is within the access definition, then a number without text imports the default
# error code information
            if ($self->{currenttext} =~ m/[a-z0-9]/i)
            {
                $self->{currentaccess}->{returncodes}->{$self->{currenterrorcode}} = $self->{currenttext};
            }
            else
            {
                if (exists($self->{data}->{_defaultrc}->{$self->{currenterrorcode}}))
                {
                    $self->{currentaccess}->{returncodes}->{$self->{currenterrorcode}} = 
                        $self->{data}->{_defaultrc}->{$self->{currenterrorcode}};
                }
                else
                {
                    $self->meta_error("Default error code text for $self->{currenterrorcode} not found");
                }
            }
        }
        $self->reset_parser_charbuffer();
    }
    elsif ($element->{Name} eq 'access')
    {
        foreach my $tag (qw/description request response/)
        {
            $self->meta_error("Missing content for \u$tag for $self->{currentaccess}->{method}: $self->{data}->{paths}->{$self->{currentid}}->{path} tag")
                unless (exists($self->{currentaccess}->{$tag}) && 
                        $self->{currentaccess}->{$tag} =~ m/[a-zA-Z0-9]+/);
        }

        $self->{currentaccess}->{_so} = $self->{methodso}->{$self->{currentaccess}->{method}} || 65536;

        push(@{$self->{data}->{paths}->{$self->{currentid}}->{access}},$self->{currentaccess});
    }
    elsif ($element->{Name} eq 'urlapi')
    {
        croak "Missing path for URLAPI for $self->{currentid}" 
            if (!exists($self->{data}->{paths}->{$self->{currentid}}->{path}) || 
                !defined($self->{data}->{paths}->{$self->{currentid}}->{path}));
    }
}         

1;
