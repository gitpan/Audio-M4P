# t/07_test.t - check for memory leaks

use strict;
use warnings;

use Test::More;

eval "use Test::Memory::Cycle";
plan skip_all => "Test::Memory::Cycle required for testing memory leaks" if $@;

plan tests => 10;

use Audio::M4P::QuickTime;

my $qt;
for ( 1 ... 10 ) {
    $qt = new Audio::M4P::QuickTime( file => 't/booka.mp4' );
    memory_cycle_ok($qt);
    $qt = '';
}

