package Audio::M4P::Decrypt;

require 5.006;
use strict;
use warnings;
use Carp;
our $VERSION = '0.31';

use Crypt::Rijndael;
use Digest::MD5;
use Audio::M4P::QuickTime;

sub new {
    my ( $class, %args ) = @_;
    my $self = {};
    bless( $self, $class );
    $self->{meta} = {};
    foreach my $k (qw( strHome sPfix dirSep DEBUG DEBUGDUMPFILE )) {
        $self->{$k} = $args{$k} if exists $args{$k};
    }
    unless ( exists $self->{strHome} ) {
        if    ( $ENV{APPDATA} ) { $self->{strHome} = $ENV{APPDATA} }
        elsif ( $ENV{HOME} )    { $self->{strHome} = $ENV{HOME} }
        else { $self->{strHome} = '~' }
    }
    unless ( exists $self->{sPfix} ) {
        if ( $^O =~ /Win/ ) { $self->{sPfix} = '' }
        else { $self->{sPfix} = '.' }
    }
    $self->{dirSep} = '/' unless exists $self->{dirSep};;
    $self->{DEBUG}  = 0 unless exists $self->{DEBUG};
    $self->{QTStream} = new Audio::M4P::QuickTime(%args);
    return $self;
}

sub GetUserKey {
    my ( $self, $userID, $keyID ) = @_;
    my ( $userKey, $keyFile, $fh );

    # default userkey if atoms are 0 is tr1-th3n.y00_by3
    return "tr1-th3n.y00_by3" unless $userID && $keyID;
    $keyFile = sprintf( "%s%s%sdrms%s%08X.%03d",
        $self->{strHome}, $self->{dirSep}, $self->{sPfix}, $self->{dirSep},
        $userID, $keyID );
    open( $fh, '<', $keyFile ) or croak "Cannot open file $keyFile: $!";
    binmode $fh;
    print "Keyfile $keyFile\n" if $self->{DEBUG};
    read( $fh, $userKey, -s $keyFile ) or croak "Cannot read user keyfile: $!";
    return $userKey;
}

sub Decrypt {
    my ( $self, $cipherText, $offset, $count, $alg ) = @_;
    my $len = int( $count / 16 ) * 16;
    substr( $$cipherText, $offset, $len,
        $alg->decrypt( substr( $$cipherText, $offset, $len ) ) );
}

sub DeDRMS {
    my ( $self, $infile, $outfile ) = @_;
    $self->{QTStream}->ReadFile($infile);
    $self->{QTStream}->ParseBuffer();
    my $sampleTable = $self->{QTStream}->GetSampleTable();
    my $userKey     = $self->GetUserKey( $self->{QTStream}->{userID},
        $self->{QTStream}->{keyID} );
    my $md5 = new Digest::MD5;
    $md5->add( $self->{QTStream}->{name}, $self->{QTStream}->{iviv} );
    my $alg = new Crypt::Rijndael( $userKey, Crypt::Rijndael::MODE_CBC );
    $alg->set_iv( $md5->digest );
    $self->Decrypt(
        \$self->{QTStream}->{priv},          0,
        length( $self->{QTStream}->{priv} ), $alg
    );
    $self->{QTStream}->{priv} =~ /^itun/ or croak "Priv decryption failed.";
    my $key = substr( $self->{QTStream}->{priv}, 24, 16 );
    $alg = new Crypt::Rijndael( $key, Crypt::Rijndael::MODE_CBC );
    $alg->set_iv( substr( $self->{QTStream}->{priv}, 48, 16 ) );
    my $mdata = $self->{QTStream}->FindAtom('mdat');
    my $posit = $mdata->start + 8;

    foreach my $samplesize ( @{$sampleTable} ) {
        $self->Decrypt( $mdata->rbuf, $posit, $samplesize, $alg );
        $posit += $samplesize;
    }
    $self->{QTStream}->ConvertDrmsToMp4a();
    $self->{QTStream}->WriteFile($outfile);
}

# DeDRMS is aliased to DecryptFile
sub DecryptFile { DeDRMS(@_) }

=head1 NAME

Audio::M4P::Decrypt -- DRMS decryption of Apple iTunes style MP4 player files

=head1 DESCRIPTION
    
Originally derived from the DeDRMS.cs program by Jon Lech Johansen

=head1 SYNOPSIS

 use Audio::M4P::Decrypt;

 my $outfile = 'mydecodedfile';
 my $deDRMS = new Audio::M4P::Decrypt;
 $deDRMS->DeDRMS($mp4file, $outfile);

See also the M4pDecrypt.pl example program in this distribution.

=head1 METHODS

=over 4

=item B<new>

 my $cs = new Audio::M4P::Decrypt;

 my $cs_conparam = Audio::M4P::Decrypt->new(
     strHome => '~', sPfix => '.', dirSep => '/'
 );

Optional arguments: strHome is the directory containing the keyfile directory.
After running VLC on a .m4p file under Windows, MacOS X, and Linux, this should
be found by the module automatically (APPDATA dir under Win32, ~/ under OS X and 
Linux). sPfix is '.' for MacOS/*nix, nil with Windows. dirSep is the char that 
separates directories, often /.

For debugging purposes, use eg:
 my $cs_conparam = Audio::M4P::Decrypt->new(
     DEBUG => 1, DEBUGDUMPFILE => 'm4ptree.html'
 );

DEBUG turns on debugging output. DEBUGDUMPFILE is an output file name to dump 
an html picture of the m4p data structure. 

=item B<DeDRMS>

 my $cs = new Audio::M4P::Decrypt;
 $cs->DeDRMS('infilename', 'outfilename');

Decode infilename, write to outfilename. Reading slurps up an entire file,
so output can overwrite the same file without a problem, we hope. Backup first.

=item B<DecryptFile>

$cs->DecryptFile('infilename', 'outfilename');

More descriptive alias for the B<DeDRMS> method.

=back

=head1 B<SEE ALSO>

=over 4

=item L<LWP::UserAgent::iTMS_Client>

=back

=head1 B<NOTES>

    This software is designed to allow different but fair use of music by the 
    purchaser of the music. In no way is this software intended to facilitate 
    use of digital music files by parties other than the purchaser of the 
    originally DRM-protected material. That is Fair Use, corporate entities. 

    If you need to locate your iTMS keys, look for compatible versions of 
    JHymn or FairKeys, or use the LWP::UserAgent::iTMS_Client Perl module.

=head1 AUTHOR 

    Original C# version: copyright Jon Lech Johansen B<jon-vl@nanocrew.net>. 
    Perl version: William Herrera B<wherrera@skylightview.com>. 

=head1 SUPPORT 

Questions, feature requests and bug reports should go to 
<wherrera@skylightview.com>.

=head1 COPYRIGHT 

=over 4

Copyright (c) 2003-2005 William Herrera. All rights reserved.  
This program is free software; you can redistribute it and/or modify 
it under the same terms as Perl itself.

=back

=cut

1;
