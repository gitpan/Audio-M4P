# t/03_test.t - check metadat MP3::Tag compatibility methods & variant MP4 files

use strict;
use warnings;

use Test::More tests => 13;

BEGIN { use_ok('Audio::M4P::Decrypt'); }

my $object = new Audio::M4P::Decrypt;
isa_ok( $object, 'Audio::M4P::Decrypt' );

use Audio::M4P::QuickTime;

my $qt = new Audio::M4P::QuickTime( file => 't/booka.mp4' );
isa_ok( $qt, 'Audio::M4P::QuickTime' );

my @tags = $qt->autoinfo;
ok( scalar @tags == 7, "autoinfo tag count" );

my ( $title, $comment, $year, $genre, $genre_txt, $track, $track_ct, $album, $artist ) =
  ( "Test Title", "test comment", "1999", 18, "Rock", 5, 20, "My Album", "The Artist" );

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

$qt->WriteFile('t/temp03a.mp4');

$qt = new Audio::M4P::QuickTime( file => 't/temp03a.mp4' );

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

