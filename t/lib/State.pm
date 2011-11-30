
package State;

use VSO;

subtype 'State::Name' => 
  as      'Str',
  where   { length($_) > 0 },
  message { 'Must have a length greater than zero' };

subtype 'State::Population' =>
  as      'Int',
  where   { $_ > 0 },
  message { "Population must be greater than zero" };

has 'name' => (
  is        => 'ro',
  isa       => 'State::Name',
  required  => 1,
  'where'     => sub { m{Colorado} }
);

has 'capital' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 1,
);

has 'population' => (
  is        => 'rw',
  isa       => 'State::Population',
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

