package CouchDocs::JSON::Parser;

use strict;
use warnings;
use Data::Dumper;
use Carp;

use vars qw/@ISA/;
@ISA = ('CouchDocs');

sub new
{
    my ($self) = @_;

    my $class = ref($self) || $self;

    return bless {
        _modulename => 'JSON::Parser',
        data => {},
        captext => 0,
        currenttext => '',
        currentparent => '',
    },$class;
}

sub start_element
{
    my ($self,$element) = @_;

    if (($element->{Name} eq 'structure') ||
        ($element->{Name} eq 'substructure'))
    {
        if (!exists($element->{Attributes}->{id}))
        {
            croak("id is a required attribute to json");
        }
        $self->{currentid} = $element->{Attributes}->{id};
        if (exists($self->{data}->{$self->{currentid}}))
        {
            croak("ID $self->{currentid} already exists");
        }
        $self->{data}->{$self->{currentid}} = {
            id => $self->{currentid},
            sub => ($element->{Name} eq 'substructure' ? 1 : 0),
        };
        $self->{currentparent} = 'structure';
        $self->setbyattrib(\$self->{data}->{$self->{currentid}},$element,[qw/iv ov dv rv xrefto/]);
        $self->validate_object_version("JSON $self->{currentid}",
                                       $self->{data}->{$self->{currentid}});
    }
    elsif ($element->{Name} eq 'field')
    {
        $self->{currentfield} = {};
        $self->{currentparent} = 'field';
        if (!exists($element->{Attributes}->{id}))
        {
            croak("id is a required attribute to field");
        }
        $self->{currentfieldid} = $element->{Attributes}->{id};
        $self->setbyattrib(\$self->{currentfield},
                           $element,
                           [qw/id optional type iv ov dv rv/]);
        $self->validate_object_version("JSON field $self->{currentfieldid}",
                                       $self->{currentfield});
    }
    elsif (($element->{Name} eq 'description') ||
           ($element->{Name} eq 'fielddesc')
           )
    {
        $self->{captext} = 1;
    }
    elsif (($element->{Name} eq 'seealso') ||
           ($element->{Name} eq 'merge')
        )
    {
        if (!exists($element->{Attributes}->{id}))
        {
            croak("id is a required attribute for references");
        }
        if ($self->{currentparent} eq 'structure')
        {
            $self->{data}->{$self->{currentid}}->{$element->{Name}}->{$element->{Attributes}->{id}} = 1;
        }
        else
        {
            $self->{currentfield}->{$element->{Name}}->{$element->{Attributes}->{id}} = 1;
        }
    }
}

# Most of the validation should occur here, before the info
# is stored and written to the final structure

sub end_element
{
    my ($self,$element) = @_;

    if ($element->{Name} eq 'field')
    {
#        die("Description for field $self->{currentfieldid} is required") 
#            unless(length($self->{currentfield}->{description}) > 0);
        $self->{data}->{$self->{currentid}}->{fields}->{$self->{currentfieldid}} = $self->{currentfield};
    }
    elsif ($element->{Name} eq 'description')
    {
        $self->{data}->{$self->{currentid}}->{description} = $self->{currenttext};
#        die("Cannot have an empty description") 
#            unless ($self->{data}->{$self->{currentid}}->{description} =~ m/[a-z0-9]/i);

        $self->{currenttext} = '';
        $self->{captext} = 0;
    }
    elsif ($element->{Name} eq 'fielddesc')
    {
        $self->{currentfield}->{description} = $self->{currenttext};
        $self->{currenttext} = '';
        $self->{captext} = 0;
    }
    elsif (($element->{Name} eq 'structure') ||
           ($element->{Name} eq 'substructure')
           )
    {
        confess("Cannot have an empty description") unless(exists($self->{data}->{$self->{currentid}}->{description}));
    }
}         

1;
