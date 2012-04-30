package CouchDocs::DBCheck;

use strict;
use warnings;

use LWP::UserAgent;
use Data::Dumper;

sub new
{
    my ($self,$opts) = @_;

    my $class = ref($self) || $self;

    return bless {
        options => $opts,
        _modulename => 'CouchDocs::DBCheck',
        metacache => {},
        filecache => {},
    };
}

sub check_link
{
    my ($url) = @_;

    my $ua = LWP::UserAgent->new;

    my $trycount = 0;
    my $success = 0;
    my $retstring  = "";

    while ($trycount <= 5)
    {
        my $response = $ua->head($url);

        if ($response->is_error())
        {
            if (($response->{'_rc'} >= 301) and
                 ($response->{'_rc'} <= 303))
            {
                $retstring = sprintf(' should be redirected to %s',$response->status_line());
                $success = 0;
                last;
            }
            if (($response->{_rc} >= 400) &&
                ($response->{_rc} <= 499))
            {
                $retstring = sprintf(' fails with %s',$response->status_line());
                $success = 0;
                last;
            }
        }
        else
        {
            $success = 1;
            last;
        }
        $trycount++;
    }
    return($success,$retstring);
}

sub start_element
{
    my ($self, $element) = @_;

    if ($self->{options}->{checkulink})
    {
        if  ($element->{Name} eq 'ulink' &&
             $element->{Attributes}->{'url'} !~ m/^https/ &&
             $element->{Attributes}->{'url'} !~ m/^mailto/)
        {
            if (!exists($self->{triedulinks}->{$element->{Attributes}->{'url'}}))
            {
                $self->{triedulinks}->{$element->{Attributes}->{'url'}} = 1;

                my ($success,$errstring) = check_link($element->{Attributes}->{'url'});

                if ($success == 0)
                {
                    push(@{$self->{issuelist}},
                         {'parentid' => $self->{currsection},
                          'type' => 'ulink',
                          'class' => 'warning',
                          'base' => $self->{currentbase},
                          'text' => join('',"URL link failure ",
                                         $element->{Attributes}->{url} || 'Not specified',
                                         $errstring,
                                         "\n")});
                }
            }
        }
    }

    if ($element->{Name} =~ m/^(appendix|article|book|part|chapter|example|preface|refentry|refsection|section)$/i)
    {
        if (exists($element->{Attributes}->{id}))
        {
            push(@{$self->{sectionmap}},$element->{Attributes}->{id});
            $self->{currsection} = $element->{Attributes}->{id};
            if (exists($self->{sectionids}->{$element->{Attributes}->{id}}))
            {
                if (!exists($self->{checkskip}->{'idmissing'}))
                {
                    push(@{$self->{issuelist}},
                         {'parentid' => $self->{currsection},
                          'type' => 'idmissing',
                          'class' => 'todo',
                          'text' => "Found a duplicate reference ID ($element->{Attributes}->{id})\n"});
                }
            }
            else
            {
                $self->{sectionids}->{$element->{Attributes}->{id}} = 0;
            }
            $self->{sectionids}->{$element->{Attributes}->{id}}++;
        }
        else
        {
            if (!exists($self->{checkskip}->{'idmissing'}))
            {
                push(@{$self->{issuelist}},
                     {'parentid' => $self->{currsection},
                      'type' => 'idmissing',
                      'class' => 'todo',
                      'text' => "Found a $element->{Name} without an ID!\n"});
            }
        }
    }

}

sub end_element
{
    my ($self, $element) = @_;

}

sub characters
{
    my ($self, $element) = @_;

}

1;
