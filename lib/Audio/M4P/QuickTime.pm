package Audio::M4P::QuickTime;

require 5.004;
use strict;
use warnings;
use Carp;
use vars qw($VERSION);
$VERSION = '0.08';

use Audio::M4P::Atom;

my %meta_info_types = (
    aaid     => 1, # album artist
    '©alb'	 => 1, # album
    akid     => 1, # ? alternate id ?
	apid     => 1, # apple id
	'©art'	 => 1, # artist (performing)
    atid     => 1, # apple itunes id ?
	'©cmt'	 => 1, # comment field
    '©com'   => 1, # composer
    covr     => 1, # cover art
	cpil     => 1, # 1 if compilation => 0 if not ?
	cprt     => 1, # copyrighted material purchaser ?
	'©day'	 => 1, # date of release--often just the year
	disk     => 1, # CD set number: cut is from, disk [field 1] of [field 2]
    geid     => 1, # iTMS store ID ?
	gnre     => 1, # genre
	'©grp'	 => 1, # group(?)
	'©nam'	 => 1, # title of track
    plid     => 1, # purchase id ?
	rtng     => 1, # rating (integer)
	tmpo     => 1, # tempo (beats per minute)
	'©too'	 => 1, # encoder
	trkn     => 1, # two fields: [field 1] track num. of [field 2] total tracks
	'©wrt'	 => 1, # composer
    '----'   => 1, # itunes specific info
);

my %tag_types = (
         AAID    => 'aaid', ALBUM => '@alb', ARTIST => '@art', 
         COMMENT => '@cmt', COM   => '@com',   CPIL => 'cpil', 
         CPRT    => 'cprt', YEAR  => '@day', DISK   => 'disk', 
         GENRE   => 'gnre', GRP   => '@grp', NAM    => '@nam', 
         RTNG    => 'rtng', TMPO  => 'tmpo', TOO    => '@too',   
         TRKN    => 'TRKN', WRT   => '@wrt',  
);

my @m4p_not_m4a_atom_types = qw( sinf cnID apID atID plID geID akID ---- ); 

sub isMetaDataType {
    return $meta_info_types{shift};
}

sub new {
    my($class, %args) = @_;
    my $self = {};
    bless($self, $class);
    $self->{meta} = {};
    foreach my $k (qw( DEBUG DEBUGDUMPFILE file)) 
      { $self->{$k} = $args{$k} if $args{$k} }
    $self->{DEBUG}  ||= 0; 
    if($self->{file}) {
        $self->ReadFile($self->{file});
        $self->ParseBuffer();
    }
    return $self;
}

sub ReadFile {
    my($self, $infile) = @_;
    open(my $infh, '<', $infile) or croak "Cannot open input $infile: $!";
    binmode $infh;
    read($infh, $self->{buffer}, -s $infile) or croak "Bad file read: $!";
    close $infh;
    $self->{meta}->{filesize} = length $self->{buffer};
}

sub ParseBuffer { 
    my($self) = @_;
    $self->{atom_count} = 0;
    $self->{root} = new Audio::M4P::Atom(
      rbuf => \$self->{buffer}, 
      type =>'file',
      size => length $self->{buffer},
      start => 0,
      offset => 16,
      parent => 0,
    );
    my $fsize = length $self->{buffer};
    print "Buffer size is $fsize\n" if $self->{DEBUG};
    $self->ParseMP4Container($self->{root}->node, 0, $fsize);
    print "Found $self->{atom_count} atoms.\n" if $self->{DEBUG};
    $self->DumpTree($self->{DEBUGDUMPFILE}) if $self->{DEBUG} > 1;   
}

sub WriteFile {
    my($self, $outfile) = @_;
    open(my $outfh, '>', $outfile) or croak "Cannot open output $outfile: $!";
    binmode $outfh;
    print $outfh $self->{buffer};
    close $outfh;
}

sub ParseMP4Container {
    my($self, $parent, $posit, $end_posit) = @_;
    my $pAtom = $parent->getNodeValue() or croak "Cannot get atom from node"; 
    $posit = $pAtom->start + $pAtom->offset   unless defined $posit;
    $end_posit = $pAtom->start + $pAtom->size unless $end_posit;
    while($posit < $end_posit) {
        my $atom = new Audio::M4P::Atom( parent => $parent, 
          rbuf => \$self->{buffer}, Read => $posit );
        print $atom->type, " at $posit size ", $atom->size, "\n" if $self->{DEBUG};
        last unless $atom->size > 7; # sanity check
        $self->{atom_count}++;    
        if($atom->type    =~ /stsd/i) { $self->ParseStsd($atom) }
        elsif($atom->type =~ /drms/i) { $self->ParseDrms($atom) }
        elsif($atom->type =~ /meta/i) { $self->ParseMeta($atom) }
        elsif($atom->isContainer()) {
            $self->ParseMP4Container($atom->node, 
              $posit + $atom->offset, $posit + $atom->size - $atom->offset);
        }
        $posit += $atom->size;
    }
}

sub ParseStsd {
    my($self, $stsd) = @_;
    $self->ParseMP4Container($stsd->node, $stsd->start + 16, 
      $stsd->start + $stsd->size - 16);
}

sub ParseDrms {
    my($self, $drms) = @_;
    $self->ParseMP4Container( $drms->node, $drms->start + 36, 
      $drms->start + $drms->size - 36 );
    $self->{userID} = unpack 'N*', $self->FindAtomData('user');
    $self->{keyID}  = unpack 'N*', $self->FindAtomData('key' );
    $self->{priv} = $self->FindAtomData('priv');
    my $name = $self->FindAtomData('name');
    $self->{name} = substr( $name, 0, index($name, "\0") ); 
    $self->{iviv} = $self->FindAtomData('iviv');
}

sub ParseMeta {
    my($self, $meta) = @_;
    $self->ParseMP4Container( $meta->node, $meta->start + 12, 
      $meta->start + $meta->size - 12);
    foreach my $type ( keys %meta_info_types ) {
        my $atom = $self->FindAtom($type) or next;
        my $adata = substr($self->{buffer}, $atom->start+24, $atom->size-24);
        next if length $adata > 300;
        if($type eq 'disk') {
            my($field1, $field2) = unpack "nn", substr($self->{buffer}, 
                $atom->start + $atom->size - 4, 4);
            $adata = "Disk $field1 of $field2";
        }
        elsif($type eq 'trkn') {
            my($fld1, $fld2) = unpack "nn", substr($self->{buffer}, 
                $atom->start + $atom->size - 6, 4);
            $adata = "Track $fld1 of $fld2";
        }
        $self->{meta}->{$type} = $adata unless length $adata > 300;
    }
    print $self->MetaInfo() if $self->{DEBUG};
}

sub AtomList {
    my ($self) = @_;
    return $self->{root}->getAllRelatives();  # returns ref to list of atoms
}

sub FindAtom {
    my($self, $type) = @_;
    my @atoms = grep { $_->type =~ /$type/i } @{$self->AtomList()};
    return @atoms if wantarray;
    return unless scalar @atoms > 0;
    return $atoms[0];
}

sub FindAtomData { 
    my($self, $type) = @_;
    my $a = $self->FindAtom($type) or return;
    return $a->data;
}

sub MetaInfo {
    my($self) = @_;
    my $meta_info = '';
    while( my($mtype, $mdata) = each %{$self->{meta}} ) 
      {  $meta_info .= "Meta type $mtype, meta data $mdata\n"  }
    return $meta_info;
}

sub AtomTree {
    my($self) = @_;
    return $self->{root}->AtomTree();
}

sub DumpTree {
    my($self, $outfile) = @_;
    if( $outfile and open(my $dumpfh, ">$outfile") ) 
      { print $dumpfh $self->AtomTree(); close $dumpfh }
    else { print $self->AtomTree() }    
}

sub ConvertDrmsToMp4a {
    my($self) = @_;
    my $diff = 0;
    my $drms = $self->FindAtom('drms') or return;
    foreach my $a ( @m4p_not_m4a_atom_types ) {
        my @unwanted = $self->FindAtom($a) or next;
        foreach my $u (@unwanted) { $diff += $u->size; $u->selfDelete() }
    }
    print "Shrunk file by $diff bytes during conversion\n" if $self->{DEBUG};
    $self->FixStco($diff);
    $drms->type('mp4a');  
}

sub FixStco {
    my($self, $sinf_sz) = @_;
    my $stco = $self->FindAtom('stco') or croak "No stco atom";
    my @samples = map { $_ - $sinf_sz } unpack( "N*", substr($self->{buffer},
      $stco->start + 16, $stco->size - 16) ); 
    substr( $self->{buffer}, 
      $stco->start + 16, $stco->size - 16, pack('N*', @samples) );
}

sub GetSampleTable {
    my($self) = @_;
    my $stsz = $self->FindAtom('stsz') or croak "No stsz table found";
    my $sampleCount = unpack 'N', substr($self->{buffer}, $stsz->start + 16, 4);
    my @samples = unpack 'N*', 
      substr($self->{buffer}, $stsz->start + 20, $sampleCount * 4);
    print "$sampleCount samples.\n" if $self->{DEBUG};
    return \@samples;
}

sub DeleteAtom {
    my($self, $unwanted) = @_;
    my $atom = $self->FindAtom($unwanted) or return;
    return $atom->selfDelete();
}

sub GetMetaInfo {
    my($self) = @_;
    my %meta_tags;
    while( my($meta_tag, $type) = each %tag_types ) {
        my $atm = $self->FindAtom($type) or next;
        my $data_atom = $atm->Contained('data') or next;
        $self->{MP4Info}->{$meta_tag} = $data_atom->data;
    }
    return $self->{MP4Info};
}

sub GetMP4Info {
    my($self) = @_;
    my $meta = $self->GetMetaInfo();
    $meta->{LAYER} = 1;
    $meta->{VERSION} = 4;
    $meta->{COPYRIGHT} = (exists $meta->{CPRT}) ? 1 : 0;
    my $mdat = FindAtom('mdat');
    $self->{SIZE} = $mdat->size || 1;
    my $mvhd_data = $self->FindAtomData('mvhv');
    my($timescale, $duration);
    if($mvhd_data) {
        my @mvhd = unpack('Ca3NNNNNN', $mvhd_data);
        if($mvhd[0] == 1) {
            $timescale = $mvhd[6];
            $duration  = ($mvhd[7]*(2**32) + $mvhd[8]) / $mvhd[6];
        }
        else { 
            $timescale = $mvhd[4];
            $duration  = $mvhd[5];
        }
        $meta->{SECONDS} = int($duration / $timescale + 0.5);
        $meta->{MM} = int($meta->{SECONDS} / 60);
        $meta->{SS} = $meta->{SECONDS} % $self->{MM};
        $meta->{BITRATE} = int($meta->{SIZE} / $meta->{SECONDS} + 0.5);
    }
    return $meta;
}

sub SetMetaInfo {
    my($self, $field, $value, $delete_old, $before) = @_;
    my $type = $tag_types{$field} || lc substr($field, 0, 4);
    my $ilst = $self->FindAtom('ilst') or return;
    my $typ = $type;
    $typ =~ s/\W//g;
    my $entry = $ilst->Contained($typ);
    my $diff = 0;
    if($entry and $delete_old) {
        my @unwanted = $ilst->Contained($typ);
        foreach my $u (@unwanted) { 
            $diff += $u->size; 
            $u->selfDelete; 
        }
    }
    $ilst->insertNewMetaData($type, $value, $before);
    $diff -= length $value;
    $diff -= 24;
    $self->FixStco($diff);
}

=head1 NAME

Audio::M4P::QuickTime -- Perl module for m4p/mp4/m4a Quicktime audio files

=head1 DESCRIPTION

In late 2004 an update to iTunes made that program incompatible with decrypted 
files still containing meta information identifying the content as having been 
purchased via iTMS. This forced an update to the module Audio::M4pDecrypt 
(now renamed Audio::M4P::Decrypt), which now strips such meta information from 
the file. As a happy side effect of these changes, this module, part of the 
Audio::M4P distribution, now allows extraction and modification of meta 
information in such files, similar to the MP3::Info and MP4::Info modules.

About QuickTime File Structure and Atoms

Quicktime (MP4) files are arranged as a stream of 'atoms', each containing 
a header consisting of size and type information followedby data. The data 
may contain other Quicktime atoms. iTMS (M4P) music files are Quicktime 
audio files which are encrypted using a combination of information in the 
file's drms atom and information which is commonly stored on the computer 
or audio player. 
    

=head1 SYNOPSIS

=over 4

 use Audio::M4P::QuickTime;

 my $mp4file = "file.m4p";

 my $qt = new Audio::M4P::QuickTime(file => $mp4file);

 my $tags = $qt->GetMetaInfo;

 print "Artist is $tags->{ARTIST}\n" if $tags->{ARTIST};

=back

=head1 METHODS

=over 4

=item B<new>

 my $qt = Audio::M4P::QuickTime->new;

 $qt = new Audio::M4P::QuickTime(
   DEBUG => 2, 
   DEBUGDUMPFILE => 'quicktime_treedump.html'
 );

 $qt = new Audio::M4P::QuickTime(file => 'qt_audio_file.m4p');

Create a new Audio::M4P::QuickTime object. DEBUG => 1 as argument causes 
parse and other information to be printed to stdout during processing. 
DEBUG => 2, DEBUGDUNMPFILE => "file" causes an HTML tree representation 
of the QuickTime file to be emitted to the file given as value to the 
argument pair. file => "filename.m4p" causes the named QuickTime file to 
be read and parsed during object initialization.

=item B<ReadFile>

 $qt->ReadFile("filename.m4a");

Read the named file into the QuickTime object buffer.

=item B<ParseBuffer>

 $qt->ParseBufffer;

Parse the file that has been read as a QuickTime stream.

=item B<WriteFile>

 $qt->WriteFile("ouput.m4p");

Write the (possibly modified) file back to the output file argument.

=item B<GetMetaInfo>

 my $hashref = $qt->GetMetaInfo;
 while(my($tag, $value) = each %{$hashref}) { 
    print "$tage => $value\n";
 }

Returns a hash reference to meta tag information. Attempts to be compatible 
with tag information formats in MP3::Info and MP4::Info. Potential tags are 
AAID, ALBUM, ARTIST, COMMENT, COM, CPIL, CPRT, YEAR, DISK, GENRE, GRP, NAM,  
RTNG, TMPO, TOO, TRKN, and WRT. Note that, due to preservation of compatibility 
with MP3::Info by returning tag info as a hash reference, duplicate entries of 
the same tag name, such as multiple comment fields, will not be returned in the hash 
reference.

=item B<GetMP4Info>

 my $hashref = $qt->GetMP4Info;
 while(my($tag, $value) = each %{$hashref}) { 
    print "$tage => $value\n";
 }

Returns a hash reference to MP3 tag audio information. Attempts to be compatible 
with tag information formats in MP3::Info and MP4::Info. Potential tags are 
LAYER (1), VERSION (4), SIZE, SECONDS, SS, MM, and BITRATE.

=item B<SetMetaInfo>

 my $comment = "After paying for this music file, I have fair use rights to change it.";

 $qt->SetMetaInfo(COMMENT => $comment);
 $qt->SetMetaInfo(GENRE => "Bebop", 1, 'day');

Set a meta information field. The third argument, if given and true, indicates
that the program should replace all instances of meta data of this type with 
the new entry, rather than adding the tag to the existing meta data. The fourth 
argument, if given and true, indicated a tag value before which the new tag is
to be placed in the file.

=back

=head1 BUGS

=over 4

There ought to be a DWIM way to set iTunes M4P compatible metadata without 
pack() gymnastics.

No support in Atom.pm currently for resizing of single atoms over 
4294967296 bytes (4.2 gigabytes) in size. This should only occur with 
large movie files, not with MP4 audio.

The Audio::M4P::* code is not re-entrant, due to recursive changes to 
containers not being thread-safe. Threaded code using these modules 
may need to lock down all method calls with a semaphore or other 
serialization method.

=back

=head1 AUTHOR 

William Herrera B<wherrera@skylightview.com>. 

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
