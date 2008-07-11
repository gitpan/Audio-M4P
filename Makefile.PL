use ExtUtils::MakeMaker;

require 5.008;

WriteMakefile(
    NAME         => 'Audio::M4P',
    AUTHOR       => 'William Hererra (whererra@skylightview.com)',
    ABSTRACT     => 'Perl M4P/MP4/M4a video and audio media file tools',
    VERSION_FROM => 'lib/Audio/M4P/QuickTime.pm', # finds $VERSION
    LICENSE      => 'perl',
    PREREQ_PM    => { Crypt::Rijndael => 0, Digest::MD5 => 0, Test::More => 0,
                      Tree::Simple => 0, Tree::Simple::Visitor => 0,
                      Tree::Simple::View::HTML => 0, Scalar::Util => 0,
                      iTunes::Sid => 0.04, Crypt::AppleTwoFish => 0.05  },
);


