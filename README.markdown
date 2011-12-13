# NAME

VSO - Very Simple Objects

# SYNOPSIS

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

# DESCRIPTION

VSO aims to offer a declarative OO style for Perl with very little overhead, without
being overly-minimalist.

VSO is a simplified Perl5 object type system _similar_ to [Moose](http://search.cpan.org/perldoc?Moose), but simpler.

## TYPES

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

- Maybe[`a]

If it's a 'Maybe[whatever]', just do `required => 0`

- RoleName

VSO does not currently support 'roles'.

# AUTHOR

John Drago <jdrago_999@yahoo.com>

# LICENSE

This software is Free software and may be used and redistributed under the same
terms as perl itself.