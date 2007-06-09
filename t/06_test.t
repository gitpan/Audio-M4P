# t/07_test.t - check Audio::M4P::Atom::data method

# Note: this test file was contributed by Stefano Santoro--thanks!

use strict;
use warnings;

use Test::More tests => 11;

use Audio::M4P::QuickTime;

use constant ILST_START  => 1559;
use constant ILST_SIZE   => 9979;

use constant CNID_CONTENT => 98111347;
use constant CNID_START   => 11510;
use constant CNID_SIZE    => 28;

use constant APID_CONTENT => "plain.nobody\@nowhere.else";
use constant APID_START   => 11461;
use constant APID_SIZE    => 49;
use constant APID_REPL    => "some.ridicously.long.email.address.used.for.test.purposes\@test.it";

my $qt = new Audio::M4P::QuickTime(file => 't/emotre.m4a');
isa_ok ($qt, 'Audio::M4P::QuickTime');

my $ilst = $qt->FindAtom("ilst");
ok( $ilst->size == ILST_SIZE && $ilst->start == ILST_START, "ilst atom placement");

my $cnid = $qt->FindAtom("cnID");
ok( $cnid->size == CNID_SIZE && $cnid->start == CNID_START, "cnID atom placement");

my @cnIDContent = unpack("Na4h16N", $cnid->data);
ok( $cnIDContent[3] == CNID_CONTENT, "original cnID content");

my $apid = $qt->FindAtom("apID");
ok( $apid->size == APID_SIZE && $apid->start == APID_START, "apID atom placement");

my $diffsz  = length(APID_REPL) - length(APID_CONTENT);
my $oldsize = $apid->size;

my @apIDContent = unpack("Na4h16a*", $apid->data);
ok($apIDContent[3] eq APID_CONTENT, "original apID content");

$apIDContent[3] = APID_REPL;
$apIDContent[0] = 16 + length($apIDContent[3]);

$apid->data( pack("Na4h16a*", @apIDContent) );
$qt->FixStco(-$diffsz, $apid->start);
ok( $apid->size == ($oldsize + $diffsz), "new apID size");

@apIDContent = unpack("Na4h16a*", $apid->data);
ok($apIDContent[3] eq APID_REPL, "new apID content");

$cnid = $qt->FindAtom("cnID");
ok( $cnid->size == CNID_SIZE && $cnid->start == (CNID_START + $diffsz), "new cnID atom placement");

@cnIDContent = unpack("Na4h16N", $cnid->data);
ok( $cnIDContent[3] == CNID_CONTENT, "same cnID content");

$ilst = $qt->FindAtom("ilst");
ok( $ilst->size == (ILST_SIZE + $diffsz) && $ilst->start == ILST_START, "new ilst atom size");

$qt->WriteFile("t/datatest.m4a");

