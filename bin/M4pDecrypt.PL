#!/usr/bin/perl 

use strict;
use warnings;
use Audio::M4P::Decrypt;

my $cs = new Audio::M4P::Decrypt;

if(scalar @ARGV != 2) { die "Usage: perl progname infilename outfilename\n" }
$cs->DeDRMS($ARGV[0], $ARGV[1]);
exit;
