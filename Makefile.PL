use ExtUtils::MakeMaker;

WriteMakefile(
    NAME         => 'Audio::M4P',
    AUTHOR       => 'William Hererra (whererra@skylightview.com)',
    ABSTRACT     => 'Perl M4P/MP4/M4a audio tools, include DRMS tools',
    VERSION_FROM => 'lib/Audio/M4P/QuickTime.pm', # finds $VERSION
    PREREQ_PM    => { Crypt::Rijndael => 0, Digest::MD5 => 0, Test::More => 0,
                      Tree::Simple => 0, Tree::Simple::Visitor => 0,
                      Tree::Simple::View::HTML => 0, },
);


