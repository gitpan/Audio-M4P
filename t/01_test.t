# t/01_test.t - check module loading, etc

use Test::More tests => 8;

BEGIN { use_ok( 'Audio::M4P::Decrypt' ); }

my $object = new Audio::M4P::Decrypt;
isa_ok ($object, 'Audio::M4P::Decrypt');

use Audio::M4P::QuickTime;

my %meta_data = (
   'copyright' => "2005 Possessive Records",
   'comments' => 'a comment goes here',
   'songName' => 'Fur Elise',
   'genre' => 'BeBop',
   'trackCount' => '10',
   'playlistArtistName' => 'Everyone Got A Tale to Tell',
   'discCount' => '1',
   'artistName' => 'artsy',
   'genreId' => '29',
   'composerName' => 'Mark Twain',
   'discNumber' => '1',
   'playlistName' => 'Hoo Hoo',
   'trackNumber' => '3',
   'year' => '2005',
);

my $qt = new Audio::M4P::QuickTime(file => 't/elise.m4a');
isa_ok ($qt, 'Audio::M4P::QuickTime');

$qt->iTMS_MetaInfo(\%meta_data);
$qt->WriteFile('t/temp.m4a');
$qt = new Audio::M4P::QuickTime(file => 't/temp.m4a');
isa_ok ($qt, 'Audio::M4P::QuickTime');

my %h = %{$qt->iTMS_MetaInfo};

ok(index($h{composerName},'Mark Twain') >= 0, "Composer change"); 
ok(index($h{copyright},'Possessive R') >= 0, "Copyright change"); 

%h = %{$qt->GetMetaInfo(1)};
ok($h{DISK} =~ /.+1.+1/, "Meta as text");
ok($h{CPRT} =~ /2005/, "Meta copyright");

