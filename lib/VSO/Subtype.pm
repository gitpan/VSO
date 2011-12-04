
package
VSO::Subtype;

use strict;
use warnings 'all';
use overload
  '""'  => sub { ref($_[0]) ? shift->name : VSO::Subtype->find($_[0])->name },
  'eq'  => sub { my ($l,$r) = @_; "$l" eq "$r"; },
  'ne'  => sub { my ($l,$r) = @_; "$l" ne "$r"; };
our %types = ( );
sub new { my ($class,%args) = @_; my $s = bless \%args, $class; $s->init; $s; }
sub init
{
  my $s = shift;
  $s->{where} ||= sub { 1 };
  $types{$s->{name}} = $s;
}# end init()
sub name    { shift->{name}             }
sub as      { shift->{as}               }
sub where   { $_[0]->{where}->( $_[1] )    }
sub message { $_[0]->{message}->( $_[1] )  }

sub isa
{
  my ($s, $class) = @_;
  
  return 1 if $s eq $class;
  my $parent = $s->{as} or return;
  
  grep { $_ eq $class } $s->parents;
}# end isa()

sub parents
{
  my $s = shift;
  
  my $parent = $s->{as} or return;
  my @parents = ();
  while( 1 )
  {
    push @parents, $parent;
    $parent = $types{$parent}->{as} or last;
  }# end while()
  
  @parents;
}# end parents()

sub find { shift; exists $types{$_[0]} ? return $types{$_[0]} : return $_[0] };
sub subtype_exists { shift; exists $types{$_[0]} }

1;# return true:

