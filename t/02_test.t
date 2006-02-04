# t/02_test.t - check some mdat atom edge cases

use strict;
use warnings;

use Test::More tests => 4;

BEGIN { use_ok('Audio::M4P::Decrypt'); }

my $object = new Audio::M4P::Decrypt;
isa_ok( $object, 'Audio::M4P::Decrypt' );

use Audio::M4P::QuickTime;

my $qt = new Audio::M4P::QuickTime( file => 't/0mdat.mp4' );
isa_ok( $qt, 'Audio::M4P::QuickTime' );

$qt = new Audio::M4P::QuickTime( file => 't/64bit_mdat.mp4' );
isa_ok( $qt, 'Audio::M4P::QuickTime' );
