# t/01_test.t - check module loading, etc

use Test::More tests => 2;

BEGIN { use_ok( 'Audio::M4P::Decrypt' ); }

my $object = new Audio::M4P::Decrypt;
isa_ok ($object, 'Audio::M4P::Decrypt');

