# t/06_test.t - pod coverage testing

use strict;
use warnings;

use Test::More;

eval "use Test::Pod::Coverage tests => 1";

if($@) { 
    ok(1, "Test::Pod::Coverage needed for pod coverage tests"); 
}
else {
    pod_coverage_ok( 
      "Audio::M4P::QuickTime", "Quicktime routines documented in Pod" );
}


