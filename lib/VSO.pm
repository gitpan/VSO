
package VSO;

use strict;
use warnings 'all';
use Carp qw( confess croak );
use Scalar::Util qw( weaken );
use base 'Exporter';

our $VERSION = '0.004';

our @EXPORT = qw(
  has
  before
  after
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
    *{"$caller\::$_"} = \&{$_}
  } @EXPORT;
  push @{"$caller\::ISA"}, $class;
  
  $meta->{ $caller } ||= _new_meta();
  no warnings 'redefine';
  *{"$caller\::meta"} = sub { $meta->{$caller} };
}# end import()


sub new
{
  my ($class, %args) = @_;
  
  my $s = bless \%args, $class;
  $s->_build();
  $s->BUILD() if $s->can('BUILD');
  
  return $s;
}# end new()


sub BUILD { }


sub _build
{
  my $s = shift;
  
  my $class = ref($s);
  my $m = $class->meta();
  $m->{field_names} ||= [ sort keys %{ $m->{fields} } ];
  my $fields = $m->{fields};
  
  FIELD: foreach my $name ( @{ $m->{field_names} } )
  {
    my $props = $fields->{$name};
    if( $props->{required} && (! defined($s->{$name}) ) && (! $props->{default}) )
    {
      croak "Required param '$name' was not provided for new instance of '@{[ ref($s) ]}'"
        unless defined($s->{$name});
    }# end if()
    
    if( $props->{default} && ! exists($s->{$name}) )
    {
      if( $props->{lazy} )
      {
        next FIELD;
      }
      else
      {
        if( $props->{weak_ref} )
        {
          weaken($s->{$name} = $props->{default}->( $s ));
        }
        else
        {
          $s->{$name} = $props->{default}->( $s );
        }# end if()
      }# end if()
    }# end if()
    
    my $new_value = $s->{$name};
    if( $props->{isa} )
    {
      if( $props->{validate} )
      {
        croak "Invalid value for '$name' isn't a $props->{isa}: '$new_value'"
          unless _check_value_isa( $props->{isa}, $new_value );
      }# end if()
    }# end if()
    
    if( $props->{where} )
    {
      local $_ = $new_value;
      confess "Invalid value for property '$name': '$new_value'"
        unless $props->{where}->( $s );
    }# end if()
  }# end foreach()
}# end _build()


sub extends(@)
{
  my $class = caller;
  
  no strict 'refs';
  my $m = $class->meta();
  map {
    load_class( $_ );
    push @{"$class\::ISA"}, $_;
    my $parent_meta = $_->meta or die "Class $_ has no meta!";
    map {
      $m->{fields}->{$_} = $parent_meta->{fields}->{$_}
    } keys %{ $parent_meta->{fields} };
    map {
      $m->{triggers}->{$_} = $parent_meta->{triggers}->{$_}
    } keys %{ $parent_meta->{triggers} };
  } @_;
}# end extends()


sub before($&)
{
  my $class = caller;
  my ($name, $sub) = @_;
  my $meta = $class->meta;
  
  # Sanity:
  croak "You must define property $class.$name before adding triggers to it"
    unless exists($meta->{fields}->{$name});
  
  $meta->{triggers}->{"before.$name"} ||= [ ];
  push @{ $meta->{triggers}->{"before.$name"} }, $sub;
}# end before()


sub after($&)
{
  my $class = caller;
  my ($name, $sub) = @_;
  my $meta = $class->meta;
  
  # Sanity:
  croak "You must define property $class.$name before adding triggers to it"
    unless exists($meta->{fields}->{$name});
  
  $meta->{triggers}->{"after.$name"} ||= [ ];
  push @{ $meta->{triggers}->{"after.$name"} }, $sub;
}# end after()


sub has($;@)
{
  my $class = caller;
  my $name = shift;
  my %properties = @_;
  my $meta = $class->meta;
  
  my $props = $meta->{fields}->{$name} = {
    is        => 'rw',
    required  => 1,
    isa       => undef,
    lazy      => 0,
    weak_ref  => 0,
    validate  => 1,
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
        if( $props->{weak_ref} )
        {
          weaken($s->{$name} = $props->{default}->( $s ));
        }
        else
        {
          $s->{$name} = $props->{default}->( $s );
        }# end if()
      }# end if()
      
      return $s->{$name};
    }# end unless()
    
    if( $props->{is} eq 'ro' )
    {
      croak "Cannot change readonly property '$name'";
    }
    elsif( $props->{is} eq 'rw' )
    {
      my $new_value = shift;
      my $old_value = $s->{$name};
      
      if( (! defined($new_value)) && $props->{required} )
      {
        croak "Property '$name' cannot be null";
      }# end if()
      
      if( $props->{isa} )
      {
        if( $props->{validate} )
        {
          croak "New value for '$name' isn't a $props->{isa}: '$new_value'"
            unless _check_value_isa( $props->{isa}, $new_value );
        }# end if()
      }# end if()
      
      if( $props->{where} )
      {
        local $_ = $new_value;
        confess "Invalid value for property '$name': '$new_value'"
          unless $props->{where}->( $s );
      }# end if()
      
      if( my $triggers = $meta->{triggers}->{"before.$name"} )
      {
        map {
          $_->( $s, $new_value, $old_value );
        } @$triggers;
      }# end if()
      
      # Now change the value:
      if( $props->{weak_ref} )
      {
        weaken($s->{$name} = $new_value);
      }
      else
      {
        $s->{$name} = $new_value;
      }# end if()
      
      if( my $triggers = $meta->{triggers}->{"after.$name"} )
      {
        map {
          $_->( $s, $s->{$name}, $old_value);
        } @$triggers;
      }# end if()
      
      # Default to returning the new value:
      $new_value if defined wantarray();
    }# end if()
  };
}# end has()


sub _check_value_isa
{
  my ($isa_raw, $value) = @_;
  
  my $is_ok = 0;
  foreach my $isa ( split /\|/, $isa_raw )
  {
    last if $is_ok;
    my ($reftype, $ref_isa) = $isa =~ m{((?:Array|Hash))Ref(?:\[([^\]]+?)\])?};
    $reftype = uc($reftype);
    if( $reftype && $ref_isa )
    {
      # Make sure that it is the correct type of structure:
      next unless $reftype eq ref($value);
      
      # Make sure that the elements/values are the correct type of thing:
      if( $reftype eq 'ARRAY' )
      {
        next if grep { ! ( $_->isa( $ref_isa ) || ref($_) eq $ref_isa ) } @$value;
      }
      elsif( $reftype eq 'HASH' )
      {
        next if grep { ! ( $_->isa( $ref_isa ) || ref($_) eq $ref_isa ) } values %$value;
      }# end if()
    }# end if()
    
    # Simple checks:
    if( $isa eq 'Str' )
    {
      # No problem.
      $is_ok++;
    }
    elsif( $isa eq 'Int' )
    {
      no warnings 'numeric';
      next unless $value eq sprintf('%d', $value);
    }
    elsif( $isa eq 'Num' )
    {
      next unless $value =~ m{^\d+\.?\d*?$};
    }
    elsif( $isa eq 'CodeRef' )
    {
      next unless ref($value) eq 'CODE';
    }
    elsif( $isa eq 'HashRef' )
    {
      next unless ref($value) eq 'HASH';
    }
    elsif( $isa eq 'ArrayRef' )
    {
      next unless ref($value) eq 'ARRAY';
    }
    elsif( $isa eq 'ScalarRef' )
    {
      next unless ref($value) eq 'SCALAR';
    }
    elsif( $isa eq 'FileHandle' )
    {
      next unless UNIVERSAL::isa($value, 'IO::Handle') || ref($value) eq 'GLOB';
    }# end if()
    
    $is_ok++;
  }# end foreach()
  
  return $is_ok;
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
    eval { require $file };
    $INC{$file} ||= $file;
  }# end unless();
  $class->import(@_);
}# end load_class()

1;# return true:


=pod

=head1 NAME

VSO - Very Small Objects

=head1 SYNOPSIS

  package Plane;
  
  use VSO;
  
  has 'width' => (
    is        => 'ro',
    isa       => 'Int',
  );
  
  has 'height' => (
    is        => 'ro',
    isa       => 'Int',
  );
  
  has 'points' => (
    is        => 'rw',
    isa       => 'ArrayRef[Point2d]',
    required  => 0,
  );


  package Point2d;
  
  use VSO;
  
  has 'plane' => (
    is        => 'ro',
    isa       => 'Plane',
    weak_ref  => 1,
  );
  
  has 'x' => (
    is        => 'rw',
    isa       => 'Int',
    where     => sub {
      my $s = shift;
      $_ >= 0 && $_ <= $s->plane->width
    }
  );
  
  has 'y' => (
    is        => 'rw',
    isa       => 'Int',
    where     => sub {
      my $s = shift;
      $_ >= 0 && $_ <= $s->plane->height
    }
  );
  
  after 'x' => sub {
    my ($s, $new_value, $old_value) = @_;
    warn "Moving $s from x$old_value to x$new_value";
  }
  
  after 'y' => sub {
    my ($s, $new_value, $old_value) = @_;
    warn "Moving $s from y$old_value to y$new_value";
  }
  
  package Point3d;
  
  use VSO;
  
  extends 'Point2d';
  
  has 'z' => (
    is      => 'rw',
    isa     => 'Int',
  );


=head1 DESCRIPTION

VSO aims to offer a declarative OO style for Perl with very little overhead, without
being overly-minimalist.

B<NOTE:> This is not a drop-in replacement for Moose, Moo, Mo, Mouse or anything like that.

=head1 AUTHOR

John Drago <jdrago_999@yahoo.com>

=head1 LICENSE

This software is Free software and may be used and redistributed under the same
terms as perl itself.

=cut

