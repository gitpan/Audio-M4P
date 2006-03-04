# t/01_test.t - check module loading, meta changes, etc

use strict;
use warnings;

use Test::More tests => 14;

BEGIN { use_ok( 'Audio::M4P::Decrypt' ); }

my $object = new Audio::M4P::Decrypt;
isa_ok ($object, 'Audio::M4P::Decrypt');

use Audio::M4P::QuickTime;

my $qt = new Audio::M4P::QuickTime(file => 't/elise.m4a');
isa_ok ($qt, 'Audio::M4P::QuickTime');

my $mref = $qt->GetMetaInfo(0);
ok($mref->{GENRE} == 33, "read GENRE number");

my %meta_data = (
   copyright => "Alternative Possessive Records, 2005",
   comments => 'a comment goes here',
   songName => 'Fur Elise',
   genre => 'BeBop',
   trackCount => '10',
   playlistArtistName => 'Everyone Got A Tale to Tell',
   discCount => '1',
   artistName => 'artsy',
   composerName => 'Mark Twain',
   discNumber => '1',
   playlistName => 'Hoo Hoo',
   trackNumber => '3',
   year => '2005',
);
$qt->iTMS_MetaInfo(\%meta_data);

open(my $fp1, 't/elisepic1.jpg') or die "File not found";
binmode $fp1;
open(my $fp2, 't/elisepic2.jpg') or die "File not found";
binmode $fp2;
read($fp1, my $pic1, -s $fp1); 
read($fp2, my $pic2, -s $fp2);
close $fp1;
close $fp2;
$qt->SetMetaInfo('cprt', "\xE2\x84\x97 2005 Possessive Records", 1);
$qt->SetMetaInfo('covr', $pic1, 1);
$qt->SetMetaInfo('covr', $pic2);
$qt->WriteFile('t/temp01a.m4a');

$qt = new Audio::M4P::QuickTime(file => 't/temp01a.m4a');
isa_ok ($qt, 'Audio::M4P::QuickTime');

my $href = $qt->iTMS_MetaInfo();
ok(index($href->{composerName},'Mark Twain') >= 0, "Composer change"); 
ok(index($href->{copyright}, 'Possessive') >= 0, "Copyright change"); 

$href = $qt->GetMetaInfo(1);
ok($href->{DISK} =~ /.+1.+1/, "Meta as text");
ok($href->{CPRT} =~ /2005/, "Meta copyright");

my @covr = $qt->FindAtom('covr');
ok(scalar(@covr) == 1, "Delete and add cover art as single covr atom");
my $covr_atom = $covr[0];
my @data_atoms = $covr_atom->Contained('data');
ok(scalar @data_atoms == 2, "two cover data art entries added");

$qt->SetMetaInfo('trkn', "2 of 12", 1, 0, 1);
$qt->SetMetaInfo('cprt', "2003 My Company", 1, 0, 1);
$qt->WriteFile('t/temp01b.m4a');

$qt = new Audio::M4P::QuickTime(file => 't/temp01b.m4a');
$href = $qt->GetMetaInfo(1);
ok($href->{TRKN} == 2, "Revised track num");
ok($href->{CPRT} =~ /2003/, "Revised meta copyright");
ok($href->{GENRE} eq 'BeBop', "Genre ID");
