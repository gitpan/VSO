
package VSO;

use strict;
use warnings 'all';
use Carp 'confess';
use base 'Exporter';

our $VERSION = '0.002';

our @EXPORT = qw(
  has
  before_update
  after_update
  extends
);

my $meta = { };

sub import
{
  # Turn on strict and warnings in the caller:
  import warnings;
  $^H |= 1538;
  my $class = shift;
  my $caller = caller;
  return if $caller eq __PACKAGE__;
  no strict 'refs';
  map {
    *{"$caller\::$_"} = \&{$_};
  } @EXPORT;
  push @{"$caller\::ISA"}, $class;
  
  $meta->{ $caller } ||= _new_meta();
}# end import()


sub new
{
  my ($class, %args) = @_;
  
  my $s = bless \%args, $class;
  $s->_build();
  $s->BUILD();
  
  return $s;
}# end new()


sub BUILD { }


sub _build
{
  my $s = shift;
  
  $meta->{ ref($s) } ||= _new_meta();
  my $m = $meta->{ ref($s) };
  my $fields = $m->{fields};
  
  foreach my $name ( sort keys %$fields )
  {
    my $field = $fields->{$name};
    if( $field->{required} && (! defined($s->{$name}) ) && (! $field->{default}) )
    {
      warn "Required param '$name' was not provided for new instance of '@{[ ref($s) ]}'"
        unless defined($s->{$name});
    }# end if()
  }# end foreach()
}# end _build()


sub extends(@)
{
  my $class = caller;
  
  no strict 'refs';
  $meta->{ $class } ||= _new_meta();
  my $m = $meta->{ $class };
  map {
    load_class( $_ );
    push @{"$class\::ISA"}, $_;
    my $parent_meta = $meta->{$_} or die "Class $_ has no meta!";
    map {
      $m->{fields}->{$_} = $parent_meta->{fields}->{$_}
    } keys %{ $parent_meta->{fields} };
    map {
      $m->{triggers}->{$_} = $parent_meta->{triggers}->{$_}
    } keys %{ $parent_meta->{triggers} };
  } @_;
}# end extends()


sub before_update($@)
{
  my $class = caller;
  my ($name, $sub) = @_;
  $meta->{$class} ||= _new_meta();
  
  # Sanity:
  confess "You must define property $class.$name before adding triggers to it"
    unless exists($meta->{$class}->{fields}->{$name});
  
  $meta->{$class}->{triggers}->{"before_update_$name"} ||= [ ];
  push @{ $meta->{$class}->{triggers}->{"before_update_$name"} }, $sub;
}# end before_update()


sub after_update($@)
{
  my $class = caller;
  my ($name, $sub) = @_;
  $meta->{$class} ||= _new_meta();
  
  # Sanity:
  confess "You must define property $class.$name before adding triggers to it"
    unless exists($meta->{$class}->{fields}->{$name});
  
  $meta->{$class}->{triggers}->{"after_update_$name"} ||= [ ];
  push @{ $meta->{$class}->{triggers}->{"after_update_$name"} }, $sub;
}# end after_update()


sub has($;@)
{
  my $class = caller;
  my $name = shift;
  my %properties = @_;
  $meta->{$class} ||= _new_meta();
  
  my $props = $meta->{$class}->{fields}->{$name} = {
    is        => 'rw',
    required  => 1,
    isa       => 'Any',
    %properties
  };
  
  no strict 'refs';
  *{"$class\::$name"} = sub {
    my $s = shift;
    
    # Getter:
    unless( @_ )
    {
      # Support laziness:
      if( ( ! defined($s->{$name}) ) && $props->{default} )
      {
        $s->{$name} = $props->{default}->( $s );
      }# end if()
      
      return $s->{$name};
    }# end unless()
    
    if( $props->{is} eq 'ro' )
    {
      confess "Cannot change readonly property '$name'";
    }
    elsif( $props->{is} eq 'rw' )
    {
      my $new_value = shift;
      
      if( (! defined($new_value)) && $props->{required} )
      {
        confess "Property '$name' cannot be null";
      }# end if()
      
      if( $props->{isa} )
      {
        confess "New value for '$name' isn't a $props->{isa}: '$new_value'"
          unless _check_value_isa( $props->{isa}, $new_value );
      }# end if()
      
      if( $props->{where} )
      {
        confess "Invalid value for property '$name': '$new_value'"
          unless $props->{where}->( $new_value );
      }# end if()
      
      if( my $triggers = $meta->{$class}->{triggers}->{"before_update_$name"} )
      {
        map {
          $_->( $s, $new_value, $s->{$name} );
        } @$triggers;
      }# end if()
      
      # Mark our changes:
      $s->{__Changed}->{$name}++;
      $s->{__Changes}->{$name} = $s->{$name};
      
      # Now change the value:
      $s->{$name} = $new_value;
      
      if( my $triggers = $meta->{$class}->{triggers}->{"after_update_$name"} )
      {
        map {
          $_->( $s, $s->{$name}, $new_value );
        } @$triggers;
      }# end if()
      
      # Default to returning the new value:
      $new_value if defined wantarray();
    }# end if()
  };
}# end has()


sub _check_value_isa
{
  my ($isa, $value) = @_;
  
  my ($reftype, $ref_isa) = $isa =~ m{((?:Array|Hash))Ref\[([^\]]+?)\]};
  if( $reftype )
  {
    # Make sure that it is the correct type of structure:
    return unless uc($reftype) eq ref($value);
    # Make sure that 
  }
  else
  {
    return 0 if ref($value) =~ m{^(ARRAY|HASH|SCALAR)};
  }# end if()
  
  # Simple checks:
  if( $isa eq 'Str' )
  {
    # No problem.
  }
  elsif( $isa eq 'Int' )
  {
    return unless $value =~ m{^\d+$};
  }
  elsif( $isa eq 'Num' )
  {
    return unless $value =~ m{^\d+\.?\d*?$};
  }
  elsif( $isa eq 'CodeRef' )
  {
    return unless ref($value) eq 'CODE';
  }
  elsif( $isa eq 'FileHandle' )
  {
    return unless UNIVERSAL::isa($value, 'IO::Handle') || ref($value) eq 'GLOB';
  }# end if()
  
  # More complicated checks:
  
  # Finally:
  return 1;
}# end _check_value_isa()


sub _new_meta
{
  return {
    fields  => { },
    triggers  => { }
  };
}# end _new_meta()


sub load_class
{
  my $class = shift;

  (my $file = "$class.pm") =~ s|::|/|g;
  unless( $INC{$file} ) {
    require $file;
  }# end unless();
  $class->import(@_);
}# end load_class()

1;# return true:


=pod

=head1 NAME

VSO - Very Small Objects

=head1 SYNOPSIS

  # TBD

=head1 DESCRIPTION

VSO is L<Moose>-ish, offers slightly more than L<Mo> and doesn't strive for Moose-compatibility.

Moose, Moo and Mo didn't quite do what I needed, so here's VSO.

"What did I need?" you say?  I need something that I can make millions of instances
of without wondering about side-effects.

I<(Ducks, runs away...)>

=head1 AUTHOR

John Drago <jdrago_999@yahoo.com>

=head1 LICENSE

This software is Free software and may be used and redistributed under the same
terms as perl itself.

=cut

