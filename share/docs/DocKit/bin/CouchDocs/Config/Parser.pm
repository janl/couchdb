package CouchDocs::Config::Parser;

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
        _modulename => 'Config::Parser',
        data => {},
        captext => 0,
        currenttext => '',
        currentparent => '',
    },$class;
}

sub start_element
{
    my ($self,$element) = @_;

    if ($element->{Name} eq 'configclass')
    {
        $self->{data}->{classes}->{$element->{Attributes}->{id}} = {
            description => ''
        };

        $self->setbyattrib(\$self->{data}->{classes}->{$element->{Attributes}->{id}},
                           $element,
                           [qw/iv ov dv rv/]);
        $self->{currentid} = $element->{Attributes}->{id};
        $self->{currentdescparent} = 'classes';
    }
    elsif ($element->{Name} eq 'configopt')
    {
        $self->{currentid} = $element->{Attributes}->{id};

        $self->{data}->{options}->{$self->{currentid}} = {
            class => $element->{Attributes}->{class},
            id => $self->{currentid},
            description => '',
            values => [],
        };

        $self->setbyattrib(\$self->{data}->{options}->{$self->{currentid}},
                           $element,
                           [qw/iv ov dv rv/]);

        $self->{data}->{optionsbyclass}->{$element->{Attributes}->{class}}->{$self->{currentid}} = 1;

        $self->{currentdescparent} = 'options';
    }
    elsif (($element->{Name} eq 'description')
           )
    {
        $self->{captext} = 1;
    }
    elsif (($element->{Name} eq 'valuespec')
        )
    {
        
    }
}

# Most of the validation should occur here, before the info
# is stored and written to the final structure

sub end_element
{
    my ($self,$element) = @_;

    if ($element->{Name} eq 'description')
    {
        $self->{data}->{$self->{currentdescparent}}->{$self->{currentid}}->{description} = $self->{currenttext};

        $self->{currenttext} = '';
        $self->{captext} = 0;
    }
    elsif ($element->{Name} eq 'default')
    {
        $self->{currentvaluespec}->{default} = $self->{currenttext};
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
