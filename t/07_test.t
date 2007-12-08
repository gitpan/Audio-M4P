# t/07_test.t - check for memory leaks

use strict;
use warnings;

use Test::More tests => 1;

SKIP: {

    eval { require Devel::Leak };
    skip "Devel::Leak not installed", 1 if $@;

    use Audio::M4P::QuickTime;

    my $count = Devel::Leak::NoteSV( my $handle );

    my $qt;
    for ( 1 ... 7 ) {
        $qt = new Audio::M4P::QuickTime( file => 't/booka.mp4' );
        $qt = '';
    }
       
    my $new_count = Devel::Leak::CheckSV($handle);

    warn "old object count is $count, new count is $new_count";

    ok( $new_count < $count + 800, "circular reference garbage collection" );


}
