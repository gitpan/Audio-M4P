# t/04_test.t - check creating a meta atom hierachy with ilst from scratch

use strict;
use warnings;

use Test::More tests => 14;

use Audio::M4P::QuickTime;

my $qt = new Audio::M4P::QuickTime( file => 't/elise.m4a' );
isa_ok( $qt, 'Audio::M4P::QuickTime' );

$qt->DeleteAtomWithStcoFix('meta');
ok( !$qt->FindAtom('ilst'), "ilst atom removed with meta" );

my (
    $title, $comment,  $year,  $genre, $genre_txt,
    $track, $track_ct, $album, $artist
  )
  = (
    "Fourth (Fur Elise) Test Title",
    "test comment", "1999", 18, "Rock", 5, 20, "My Album", "The Artist"
  );

$qt->album($album);
$qt->artist($artist);
$qt->title($title);
$qt->comment($comment);
$qt->year($year);
$qt->genre(18);
$qt->tracks( $track, $track_ct );

open( my $fp1, 't/feel.jpg' ) or die "File not found";
binmode $fp1;
read( $fp1, my $pic1, -s $fp1 );

$qt->SetMetaInfo( 'covr', $pic1, 1 );

$qt->WriteFile('t/temp04.m4a');

$qt = new Audio::M4P::QuickTime( file => 't/temp04.m4a' );

ok( $qt->album   eq $album,   "Album Tag" );
ok( $qt->artist  eq $artist,  "Artist Tag" );
ok( $qt->comment eq $comment, "Comment Tag" );
ok( $qt->title   eq $title,   "Title Tag" );
ok( $qt->year == $year,   "Date Tag" );
ok( $qt->genre == $genre, "Genre Numeric Tag" );
ok( $qt->genre_as_text eq $genre_txt, "Genre Text Tag" );
my ( $t, $tt ) = $qt->tracks;
ok( $t == $track,     "Tracks Tags : track number" );
ok( $tt == $track_ct, "Tracks Tags : track count" );


ok( scalar @{$qt->GetCoverArt()} == 1,  "added artwork as meta info" );
$qt->DeleteAllCoverArt;
$qt->WriteFile('t/temp04.m4a');
$qt = new Audio::M4P::QuickTime( file => 't/temp04.m4a' );
ok( ! defined $qt->GetCoverArt(),  "deleted artwork" );

$qt->AddCoverArt( $pic1, 13 );
$qt->WriteFile('t/temp04.m4a');
$qt = new Audio::M4P::QuickTime( file => 't/temp04.m4a' );
ok( scalar @{$qt->GetCoverArt()} == 1,  "added artwork as cover art" );

