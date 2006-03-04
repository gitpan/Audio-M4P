# t/02_test.t - check some mdat atom edge cases

use strict;
use warnings;

use Test::More tests => 6;

BEGIN { use_ok('Audio::M4P::Decrypt'); }

my $object = new Audio::M4P::Decrypt;
isa_ok( $object, 'Audio::M4P::Decrypt' );

use Audio::M4P::QuickTime;

my $qt = new Audio::M4P::QuickTime( file => 't/0mdat.mp4' );
isa_ok( $qt, 'Audio::M4P::QuickTime' );

$qt = new Audio::M4P::QuickTime( file => 't/64bit_mdat.mp4' );
isa_ok( $qt, 'Audio::M4P::QuickTime' );

$qt = new Audio::M4P::QuickTime( file => 't/64bit_mutt.mp4' );
isa_ok( $qt, 'Audio::M4P::QuickTime' );
my $alb = "The Mutt Album";
$qt->album($alb);
$qt->WriteFile('t/temp02.mp4');
$qt = new Audio::M4P::QuickTime( file => 't/temp02.mp4' );
ok( $qt->album eq $alb, "Album Tag" );
