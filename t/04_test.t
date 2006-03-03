# t/04_test.t - check creating a meta atom hierachy with ilst from scratch

use strict;
use warnings;

use Test::More tests => 11;

use Audio::M4P::QuickTime;

my $qt = new Audio::M4P::QuickTime( file => 't/elise.m4a' );
isa_ok( $qt, 'Audio::M4P::QuickTime' );

my $atm = $qt->FindAtom('meta');
my $pos = $atm->start;
my $siz = $atm->size;
$atm->selfDelete;
$qt->FixStco( $siz, $pos );
$atm = $qt->FindAtom('meta');
ok( !defined($atm), "Removed meta atom ok");

my ( $title, $comment, $year, $genre, $genre_txt, $track, $track_ct, $album, $artist ) =
  ( "Fourth (Fur Elise) Test Title", "test comment", "1999", 18, "Rock", 5, 20, "My Album", "The Artist" );

$qt->album($album);
$qt->artist($artist);
$qt->title($title);
$qt->comment($comment);
$qt->year($year);
$qt->genre(18);
$qt->tracks( $track, $track_ct );

open(my $fp1, 't/feel.jpg') or die "File not found";
binmode $fp1;
read($fp1, my $pic1, -s $fp1); 
$qt->SetMetaInfo('covr', $pic1, 1);

$qt->WriteFile('t/temp5.mp4');

$qt = new Audio::M4P::QuickTime( file => 't/temp5.mp4' );

ok( $qt->album   eq $album, "Album Tag" );
ok( $qt->artist  eq $artist, "Artist Tag" );
ok( $qt->comment eq $comment, "Comment Tag" );
ok( $qt->title   eq $title,   "Title Tag" );
ok( $qt->year   ==  $year,    "Date Tag" );
ok( $qt->genre == $genre, "Genre Numeric Tag" );
ok( $qt->genre_as_text eq $genre_txt, "Genre Text Tag" );
my ( $t, $tt ) = $qt->tracks;
ok( $t == $track,     "Tracks Tags : track number" );
ok( $tt == $track_ct, "Tracks Tags : track count" );
