
package MooseState;

use Moose;

has 'name' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 1,
);

has 'capital' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 1,
);

has 'population' => (
  is        => 'rw',
  isa       => 'Int',
  required  => 1,
);

has 'foo' => (
  is        => 'ro',
  isa       => 'HashRef[Foo]',
  required  => 1,
) if 0;

has 'func' => (
  is        => 'ro',
  isa       => 'CodeRef|Str',
  required  => 1,
) if 0;

before 'population' => sub {
  my ($s, $new_value, $old_value) = @_;
  
  warn "About to change population from '$old_value' to '$new_value'\n";
};

after 'population' => sub {
  my ($s, $new_value, $old_value) = @_;
  
  warn "Changed population from '$old_value' to '$new_value'\n";
};

__PACKAGE__->meta->make_immutable();

