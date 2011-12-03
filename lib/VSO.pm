
package VSO;

use strict;
use warnings 'all';
use Carp qw( confess croak );
use Scalar::Util qw( weaken openhandle );
use Data::Dumper;
use base 'Exporter';

our $VERSION = '0.010';

our @EXPORT = qw(
  has
  before
  after
  extends
  
  subtype as where message
  coerce from via
);

my $_meta = { };

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
  push @{"$caller\::ISA"}, $class if $class eq __PACKAGE__;
  
  $_meta->{ $caller } ||= _new_meta();
  no warnings 'redefine';
  *{"$caller\::meta"} = sub { $_meta->{$caller} };
}# end import()


sub new
{
  my ($class, %args) = @_;
  
  my $s = bless \%args, $class;
  $s->_build();
  $s->BUILD() if $s->can('BUILD');
  
  return $s;
}# end new()


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
      _check_type_exists( $props->{isa} );
      
      if( $props->{required} )
      {
        my $check = _check_value_isa( $props->{isa}, $s, $new_value );
        unless( $check eq 1 )
        {
          if( $props->{coerce} )
          {
            # Figure out what time we've got:
            my $valuetype = _discover_type( $new_value );
            if( my $coercer = $_meta->{_coercions}->{$props->{isa}}->{$valuetype} )
            {
              local $_ = $new_value;
              $new_value = $coercer->( $s );
            }
            else
            {
              confess "Invalid value for '$name' isn't a $props->{isa}: '$new_value'@{[ $check eq 0 ? '' : qq(: $check) ]}";
            }# end if()
          }
          else
          {
            confess "Invalid value for '$name' isn't a $props->{isa}: '$new_value'@{[ $check eq 0 ? '' : qq(: $check) ]}";
          }# end if()
        }# end unless()
      }# end if()
    }# end if()
    
    if( $props->{where} && defined($new_value) )
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
    unless exists($meta->{fields}->{$name}) || $class->can($name);
  
  if( exists($meta->{fields}->{$name}) )
  {
    $meta->{triggers}->{"before.$name"} ||= [ ];
    push @{ $meta->{triggers}->{"before.$name"} }, $sub;
  }
  else
  {
    my $orig = $class->can($name);
    no strict 'refs';
    no warnings 'redefine';
    *{"$class\::$name"} = sub {
      $sub->( @_ );
      $orig->( @_ );
    };
  }# end if()
}# end before()


sub after($&)
{
  my $class = caller;
  my ($name, $sub) = @_;
  my $meta = $class->meta;
  
  # Sanity:
  croak "You must define property $class.$name before adding triggers to it"
    unless exists($meta->{fields}->{$name}) || $class->can($name);
  
  if( exists($meta->{fields}->{$name}) )
  {
    $meta->{triggers}->{"after.$name"} ||= [ ];
    push @{ $meta->{triggers}->{"after.$name"} }, $sub;
  }
  else
  {
    my $orig = $class->can($name);
    no strict 'refs';
    no warnings 'redefine';
    *{"$class\::$name"} = sub {
      my $context = defined(wantarray) ? wantarray ? 'list' : 'scalar' : 'void';
      my ($res,@res);
      $context eq 'list' ? @res = $orig->( @_ ) : $context eq 'scalar' ? $res = $orig->( @_ ) : $orig->( @_ );
      $sub->( @_ );
      $context eq 'list' ? return @res : $context eq 'scalar' ? return $res : return;
    };
  }# end if()
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
    coerce    => 0,
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
        my $check = _check_value_isa( $props->{isa}, $s, $new_value );
        unless( $check eq 1 )
        {
          if( $props->{coerce} )
          {
            # Figure out what time we've got:
            my $valuetype = _discover_type( $new_value );
            if( my $coercer = $_meta->{_coercions}->{$props->{isa}}->{$valuetype} )
            {
              local $_ = $new_value;
              $new_value = $coercer->( $s );
            }
            else
            {
              confess "Invalid value for '$name' isn't a $props->{isa}: '$new_value'@{[ $check eq 0 ? '' : qq(: $check) ]}";
            }# end if()
          }
          else
          {
            croak "New value for '$name' isn't a $props->{isa}: '$new_value'@{[ $check eq 0 ? '' : qq(: $check) ]}";
          }# end if()
        }# end unless()
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


CLOSURE: {
  my %parents = ( );
  sub _check_value_isa
  {
    my ($isa_raw, $object, $value) = @_;
    
    my ($outer_type, $inner_type) = _parse_typename($isa_raw);
    if( my $subtype = _find_subtype($isa_raw) )
    {
      my @parents = ( );
      if( $parents{$isa_raw} )
      {
        @parents = @{ $parents{$isa_raw} };
      }
      else
      {
        my $get_parents; $get_parents = sub {
          return unless $_[0]->{as};
          my ($outer_type, $inner_type) = _parse_typename(shift->{as});
          my $parent = _find_subtype($outer_type, $inner_type)
            or return @parents;
          push @parents, $parent;
          $get_parents->( $parent );
        };
        $get_parents->( $subtype );
        $parents{$isa_raw} = \@parents;
      }# end if()
      my @type_strata = ( (reverse @parents), $subtype );
      map {
        my $type = $_;
        local $_ = $value;
        return $type->{message}->( $object )
          unless $type->{where}->( $object );
      } @type_strata;

      return 1;
    }
    else
    {
      return "Unknown type '$isa_raw'";
    }# end if()
    
    return 1;
  }# end _check_value_isa()
};


sub _parse_typename
{
  my $typename = shift;
  
  if( my ($reftype,$valtype) = $typename =~ m{^((?:Array|Hash)Ref)\[(.+?)\]$} )
  {
    return ($reftype, $valtype);
  }# end if()
  
  return $typename;
}# end _parse_typename()


sub _discover_type
{
  my ($val) = @_;
  
  if( my $ref = ref($val) )
  {
    return 'ScalarRef' if $ref eq 'SCALAR';
    return 'ArrayRef' if $ref eq 'ARRAY';
    return 'HashRef' if $ref eq 'HASH';
    return 'CodeRef' if $ref eq 'CODE';
    return 'GlobRef' if $ref eq 'GLOB';
    return 'RegexpRef' if $ref eq 'Regexp';
    return 'FileHandle' if openhandle($val);
    # Otherwise, it's a reference to some kind of object:
    return $ref;
  }
  else
  {
    return 'Undef' unless defined($val);
    return 'Defined' if defined($val) && ! length($val);
    return 'Bool' if $val =~ m{^(?:0|1)$};
    return 'Int' if $val =~ m{^\d+$};
    return 'Num' if $val =~ m{^\d+\.?\d*?$};
    # ClassName?:
    (my $fn = "$val.pm") =~ s{::}{/}g;
    return 'ClassName' if exists($INC{$fn});
    return 'Str';
  }# end if()
}# end _discover_type()


sub _check_type_exists
{
  my $typename = shift;
  
  if( my ($reftype,$valtype) = $typename =~ m{^((?:Array|Hash)Ref)\[(.+?)\]$} )
  {
    (my $fn = "$valtype.pm") =~ s{::}{/}g;
    if( $valtype )
    {
      $_meta->{_subtypes}->{$valtype} ||= {
        name    => $valtype,
        as      => 'Object',
        where   => sub { 1 },
        message => sub { "Must be a '$valtype'" }
      };
    }# end if()
    if( $reftype eq 'HashRef' )
    {
      $_meta->{_subtypes}->{$typename} ||= {
        name    => $typename,
        as      => $reftype,
        where   => sub {
          my $val = $_;
          my $obj = shift;
          my (@vals) = values %$val;
          my $failed = grep {
            _check_value_isa( $valtype, undef, $_ ) eq 1;
          } @vals;
          $failed ? return 0 : return 1;
        },
        message => sub { "Must be a '$typename'" }
      };
    }
    else
    {
      # ArrayRef:
      $_meta->{_subtypes}->{$typename} ||= {
        name    => $typename,
        as      => $reftype,
        where   => sub {
          my $failed = grep {
            my $result = _check_value_isa( $valtype, undef, $_ );
            $result;
          } @$_;
          $failed ? return 0 : return 1;
        },
        message => sub { "Must be a $typename" }
      };
    }# end if()
  }
  else
  {
    # Normal class:
    unless( _find_subtype($typename) )
    {
      load_class($typename);
      $_meta->{_subtypes}->{$typename} ||= {
        name    => $typename,
        as      => 'Object',
        where   => sub { 1 },
        message => sub { "Must be a '$typename'" }
      };
    }# end unless()
  }# end if()
}# end _check_type_exists()


CLOSURE: {
  my %subtype_cache = ( );
  sub _find_subtype
  {
    my ($typename, $valname) = @_;
    
    my $key = join ':', ( grep { defined } ( $typename, $valname ) );
    exists $subtype_cache{$key} ?
      defined($subtype_cache{$key}) ?
        return $subtype_cache{$key}
        : return
        : 0; # NO-OP:
    my ($match) = grep {
      $_ eq $typename
    } sort {length($b) <=> length($a)} keys %{$_meta->{_subtypes}};
    
    $subtype_cache{$key} = $_meta->{_subtypes}->{$match}
      if $match;
    $match ? return $subtype_cache{$key} : return;
  }# end _find_subtype()
};


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
#  unless( $INC{$file} ) {
    eval { require $file };
    $INC{$file} ||= $file;
#  }# end unless();
  $class->import(@_);
}# end load_class()


sub subtype($;@)
{
  my ($name, %args) = @_;
  
  confess "Subtype '$name' already exists"
    if exists($_meta->{_subtypes}->{$name});
  $_meta->{_subtypes}->{$name} = {
    name    => $name,
    as      => $args{as},
    where   => $args{where} || sub { 1 },
    message => $args{message} || sub { "Must be a valid '$name'" },
  };
}# end subtype()
sub as          { as => shift, @_   }
sub where(&)    { where => $_[0]    }
sub message(&)  { message => $_[0]  }


sub coerce($;@)
{
  my ($to, %args) = @_;
  
  confess "Unknown type '$to'" unless exists $_meta->{_subtypes}->{$to};
  confess "Unknown type '$args{from}'" unless $_meta->{_subtypes}->{$args{from}};
  my ($pkg,$filename,$line) = caller;
  confess "Coercion from '$args{from}' to '$to' is already defined in $filename line $line"
    if defined($_meta->{_coercions}->{$to}->{$args{from}});
  $_meta->{_coercions}->{$to}->{$args{from}} = $args{via};
}# end coerce()
sub from    { from => shift, @_ }
sub via(&)  { via  => $_[0]     }


# All things spring forth from the formless void:
$_meta->{_subtypes}->{''} = {
  as      => '',
  where   => sub { 1 },
  message => sub { '' }
};

subtype 'Any' =>
  as      '',
  where   { 1 },
  message { '' };

subtype 'Item'  =>
  as      '',
  where   { 1 },
  message { '' };

  subtype 'Bool' =>
    as      'Item',
    where   { m{^(?:1|0)$} },
    message { "Must be a 1 or a 0" };

  subtype 'Undef' =>
    as      'Item',
    where   { ! defined },
    message { "Must not be defined" };

  subtype 'Defined' =>
    as      'Item',
    where   { defined },
    message { "Must be defined" };

    subtype 'Value' =>
      as      'Defined',
      where   { ! ref },
      message { "Cannot be a reference" };

      subtype 'Str' =>
        as      'Value',
        where   { 1 },
        message { '' };

        subtype 'Num' =>
          as      'Str',
          where   { m{^\d+\.?\d*?$} },
          message { 'Must contain only numbers and decimals' };

          subtype 'Int' =>
            as      'Num',
            where   { m{^\d+$} },
            message { 'Must contain only numbers 0-9' };

        subtype 'ClassName' =>
          as      'Str',
          where   { m{^[a-z\:0-9_]+$}i },
          message { 'Must match m{^[a-z\:0-9_]+$}i' };

    subtype 'Ref' =>
      as      'Defined',
      where   { ref },
      message { 'Must be a reference' };

      subtype 'ScalarRef' =>
        as      'Ref',
        where   { ref($_) eq 'SCALAR' },
        message { 'Must be a scalar reference (ScalarRef)' };

      subtype 'ArrayRef'  =>
        as      'Ref',
        where   { ref($_) eq 'ARRAY' },
        message { 'Must be an array reference (ArrayRef)' };

      subtype 'HashRef' =>
        as      'Ref',
        where   {ref($_) eq 'HASH' },
        message { 'Must be a hash reference (HashRef)' };

      subtype 'CodeRef' =>
        as      'Ref',
        where   { ref($_) eq 'CODE' },
        message { 'Must be a code reference (CodeRef)' };

      subtype 'RegexpRef' =>
        as      'Ref',
        where   { ref($_) eq 'Regexp' },
        message { 'Must be a Regexp' };

      subtype 'GlobRef' =>
        as      'Ref',
        where   { ref($_) eq 'GLOB' },
        message { 'Must be a GlobRef (GLOB)' };
      
        subtype 'FileHandle'  =>
          as      'GlobRef',
          where   { openhandle($_) },
          message { 'Must be a FileHandle' };
          
      subtype 'Object'  =>
        as      'Ref',
        where   { no strict 'refs'; scalar(@{ref($_) . "::ISA"}) },
        message { 'Must be an object' };

1;# return true:


=pod

=head1 NAME

VSO - Very Simple Objects

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
  
  subtype 'ValidValue' =>
    as      'Int',
    where   { $_ >= 0 && $_ <= shift->plane->width },
    message { 'Value must be between zero and ' . shift->plane->width };
  
  has 'plane' => (
    is        => 'ro',
    isa       => 'Plane',
    weak_ref  => 1,
  );
  
  has 'x' => (
    is        => 'rw',
    isa       => 'ValidValue'
  );
  
  has 'y' => (
    is        => 'rw',
    isa       => 'ValidValue'
  );
  
  after 'x' => sub {
    my ($s, $new_value, $old_value) = @_;
    warn "Moving $s from x$old_value to x$new_value";
  };
  
  after 'y' => sub {
    my ($s, $new_value, $old_value) = @_;
    warn "Moving $s from y$old_value to y$new_value";
  };
  
  package Point3d;
  
  use VSO;
  
  extends 'Point2d';
  
  has 'z' => (
    is      => 'rw',
    isa     => 'Int',
  );

  sub greet { warn "Hello, World!" }
  
  before 'greet' => sub {
    warn "About to greet you";
  };
  
  after 'greet' => sub {
    warn "I have greeted you";
  };

=head1 DESCRIPTION

VSO aims to offer a declarative OO style for Perl with very little overhead, without
being overly-minimalist.

VSO is a simplified Perl5 object type system similar to L<Moose>, but simpler.

=head2 TYPES

VSO offers the following type system:

  Any
  Item
      Bool
      Undef
      Defined
          Value
              Str
                  Num
                      Int
                  ClassName
          Ref
              ScalarRef
              ArrayRef
              HashRef
              CodeRef
              RegexpRef
              GlobRef
                  FileHandle
              Object

Missing from the Moose type system are:

=over 4

=item Maybe[`a]

B<Rationale:> If it's a 'Maybe[whatever]', just do C<< required => 0 >>

=item RoleName

B<Rationale:> VSO does not currently support 'roles'.

=back

=head1 AUTHOR

John Drago <jdrago_999@yahoo.com>

=head1 LICENSE

This software is Free software and may be used and redistributed under the same
terms as perl itself.

=cut

