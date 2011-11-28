
package State;

use VSO;

has 'name' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 1,
  where     => sub { m{Colorado} }
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
);

has 'func' => (
  is        => 'ro',
  isa       => 'CodeRef|Str',
  required  => 1,
);

before 'population' => sub {
  my ($s, $new_value, $old_value) = @_;
  
  warn "About to change population from '$old_value' to '$new_value'\n";
};

after 'population' => sub {
  my ($s, $new_value, $old_value) = @_;
  
  warn "Changed population from '$old_value' to '$new_value'\n";
};

1;# return true:

