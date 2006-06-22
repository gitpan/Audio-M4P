package Audio::M4P::QuickTime;

require 5.006;
use strict;
use warnings;
use Carp;
our $VERSION = '0.32';

use Audio::M4P::Atom;

#-------------- useful hashes and arrays ------------------------------------#

our %meta_info_types = (
    aaid   => 1,    # album artist
    '©alb' => 1,    # album
    akid   => 1,    # ? alternate id ?
    apid   => 1,    # apple id
    '©ART' => 1,    # artist (performing)
    atid   => 1,    # apple itunes id ?
    '©cmt' => 1,    # comment field
    '©com' => 1,    # composer
    covr   => 1,    # cover art
    cpil   => 1,    # 1 if compilation => 0 if not ?
    cprt   => 1,    # copyrighted material purchaser ?
    '©day' => 1,    # date of release--often just the year
    disk   => 1,    # CD set number: cut is from, disk [field 1] of [field 2]
    geid   => 1,    # iTMS store ID ?
    gnre   => 1,    # genre
    '©grp' => 1,    # group(?)
    '©nam' => 1,    # title of track
    plid   => 1,    # purchase id ?
    rtng   => 1,    # rating (integer)
    stik   => 1
    ,  # movie type: 0x1 default, 0x5 bookmarkable, 0x6 music video, 0xA TV show, ?? 0x2 newsreel
    tmpo   => 1,    # tempo (beats per minute)
    '©too' => 1,    # encoder
    trkn   => 1,    # two fields: [field 1] track num. of [field 2] total tracks
    '©wrt' => 1,    # composer
    '----' => 1,    # itunes specific info
);

our %tag_types = (
    AAID     => 'aaid',
    ALB      => '©alb',
    ALBUM    => '©alb',
    ARTIST   => '©ART',
    CMT      => '©cmt',
    COMMENT  => '©cmt',
    COM      => '©com',
    CPIL     => 'cpil',
    CPRT     => 'cprt',
    DAY      => '©day',
    DISK     => 'disk',
    GENRE    => 'gnre',
    GNRE     => 'gnre',
    GRP      => '©grp',
    NAM      => '©nam',
    RTNG     => 'rtng',
    SONG     => '©nam',
    TITLE    => '©nam',
    TMPO     => 'tmpo',
    TOO      => '©too',
    TRACKNUM => 'trkn',
    TRKN     => 'trkn',
    WRT      => '©wrt',
    COVR     => 'covr',
    LYRICS   => '©lyr',
    GENRE_   => '©gen',
    YEAR     => '©day',
);

our %alternate_tag_types = (
    GENRE   => 'GNRE',
    NAM     => 'TITLE',
    ARTIST  => 'ART',
    ALBUM   => 'ALB',
    YEAR    => 'DAY',
    COMMENT => 'CMT',
    TRKN    => 'TRACKNUM',
    SONG    => 'TITLE',
);

our @m4p_not_m4a_atom_types = qw( sinf cnID apID atID plID geID akID ---- );

our %iTMS_dict_meta_types = (
    copyright          => 'cprt',
    comments           => '©cmt',
    songName           => '©nam',
    genre              => 'gnre',
    playlistArtistName => '©ART',
    genreID            => '©gen',
    composerName       => '©wrt',
    playlistName       => '©alb',
    year               => '©day',
    trackNumber        => 'trkn',
    trackCount         => 'trkn',
    discNumber         => 'disk',
    discCount          => 'disk',
    artworkURL         => 'covr',
);

our @genre_strings = (
    "Blues",             "Classic Rock",
    "Country",           "Dance",
    "Disco",             "Funk",
    "Grunge",            "Hip-Hop",
    "Jazz",              "Metal",
    "New Age",           "Oldies",
    "Other",             "Pop",
    "R&B",               "Rap",
    "Reggae",            "Rock",
    "Techno",            "Industrial",
    "Alternative",       "Ska",
    "Death Metal",       "Pranks",
    "Soundtrack",        "Euro-Techno",
    "Ambient",           "Trip-Hop",
    "Vocal",             "Jazz+Funk",
    "Fusion",            "Trance",
    "Classical",         "Instrumental",
    "Acid",              "House",
    "Game",              "Sound Clip",
    "Gospel",            "Noise",
    "AlternRock",        "Bass",
    "Soul",              "Punk",
    "Space",             "Meditative",
    "Instrumental Pop",  "Instrumental Rock",
    "Ethnic",            "Gothic",
    "Darkwave",          "Techno-Industrial",
    "Electronic",        "Pop-Folk",
    "Eurodance",         "Dream",
    "Southern Rock",     "Comedy",
    "Cult",              "Gangsta",
    "Top 40",            "Christian Rap",
    "Pop/Funk",          "Jungle",
    "Native American",   "Cabaret",
    "New Wave",          "Psychadelic",
    "Rave",              "Showtunes",
    "Trailer",           "Lo-Fi",
    "Tribal",            "Acid Punk",
    "Acid Jazz",         "Polka",
    "Retro",             "Musical",
    "Rock & Roll",       "Hard Rock",
    "Folk",              "Folk/Rock",
    "National Folk",     "Swing",
    "Fast-Fusion",       "BeBop",
    "Latin",             "Revival",
    "Celtic",            "Bluegrass",
    "Avantgarde",        "Gothic Rock",
    "Progressive Rock",  "Psychedelic Rock",
    "Symphonic Rock",    "Slow Rock",
    "Big Band",          "Chorus",
    "Easy Listening",    "Acoustic",
    "Humour",            "Speech",
    "Chanson",           "Opera",
    "Chamber Music",     "Sonata",
    "Symphony",          "Booty Bass",
    "Primus",            "Porn Groove",
    "Satire",            "Slow Jam",
    "Club",              "Tango",
    "Samba",             "Folklore",
    "Ballad",            "Power Ballad",
    "Rhythmic Soul",     "Freestyle",
    "Duet",              "Punk Rock",
    "Drum Solo",         "A capella",
    "Euro-House",        "Dance Hall",
    "Goa",               "Drum & Bass",
    "Club House",        "Hardcore",
    "Terror",            "Indie",
    "BritPop",           "NegerPunk",
    "Polsk Punk",        "Beat",
    "Christian Gangsta", "Heavy Metal",
    "Black Metal",       "Crossover",
    "Contemporary C",    "Christian Rock",
    "Merengue",          "Salsa",
    "Thrash Metal",      "Anime",
    "JPop",              "SynthPop",
    "INVALID_GENRE"
);

our %asset_3GP_types = (
    ALBUM     => 'albm',      # album title and track number for the media 
    ARTIST    => 'perf',      # performer or artist 
    COM       => 'auth',      # author/composer of the media 
    COMMENT   => 'dscp',      # caption or description for the media
    COPYRIGHT => 'cprt',       # notice about organisation holding copyright
    GENRE     => 'gnre',      # genre (category and style) of the media
    RTNG      => 'rtng',       # media rating 
    TITLE     => 'titl',      # title for the media 
    YEAR      => 'yrrc',       # recording year for the media 
    
    # these exist in 3GP but not really in iTMS meta data
    CLASS     => 'clsf',      # classification of the media 
    KEYWORDS  => 'kywd',      # media keywords 
    LOCATION  => 'loci',      # location information 
);    

#------------------- object methods -----------------------------------------#

sub new {
    my ( $class, %args ) = @_;
    my $self = {};
    bless( $self, $class );
    $self->{meta} = {};
    foreach my $k (qw( DEBUG DEBUGDUMPFILE file)) {
        $self->{$k} = $args{$k} if exists $args{$k};
    }
    $self->{DEBUG} = 0 unless exists $self->{DEBUG};
    if ( exists $self->{file} ) {
        $self->ReadFile( $self->{file} );
        $self->ParseBuffer();
    }
    return $self;
}

sub ReadFile {
    my ( $self, $infile ) = @_;
    open( my $infh, '<', $infile ) or croak "Cannot open input $infile: $!";
    binmode $infh;
    read( $infh, $self->{buffer}, -s $infile ) or croak "Bad file read: $!";
    close $infh;
    $self->{meta}->{filesize} = length $self->{buffer};
}

sub ParseBuffer {
    my ($self) = @_;
    $self->{atom_count} = 0;
    $self->{root}       = new Audio::M4P::Atom(
        rbuf   => \$self->{buffer},
        type   => 'file',
        size   => length $self->{buffer},
        start  => 0,
        offset => 8,
        parent => 0,
    );
    my $fsize = length $self->{buffer};
    print "Buffer size is $fsize\n" if $self->{DEBUG};
    $self->ParseMP4Container( $self->{root}->node, 0, $fsize );
    print "Found $self->{atom_count} atoms.\n" if $self->{DEBUG};
    $self->DumpTree( $self->{DEBUGDUMPFILE} )  if $self->{DEBUG} > 1;
}

sub WriteFile {
    my ( $self, $outfile ) = @_;
    open( my $outfh, '>', $outfile ) or croak "Cannot open output $outfile: $!";
    binmode $outfh;
    print $outfh $self->{buffer};
    close $outfh;
}

sub ParseMP4Container {
    my ( $self, $parent, $posit, $end_posit ) = @_;
    my $pAtom = $parent->getNodeValue() or croak "Cannot get atom from node";
    $posit     = $pAtom->start + $pAtom->offset unless defined $posit;
    $end_posit = $pAtom->start + $pAtom->size   unless $end_posit;
    while ( $posit < $end_posit ) {
        my $atom = new Audio::M4P::Atom(
            parent      => $parent,
            rbuf        => \$self->{buffer},
            read_buffer => $posit
        );
        print $atom->type, " at $posit size ", $atom->size, "\n"
          if $self->{DEBUG};
        last unless $atom->size > 7;    # sanity check
        $self->{atom_count}++;
        if    ( $atom->type =~ /stsd/i ) { $self->ParseStsd($atom) }
        elsif ( $atom->type =~ /drms/i ) { $self->ParseDrms($atom) }
        elsif ( $atom->type =~ /meta/i ) { $self->ParseMeta($atom) }
        elsif ( $atom->isContainer() ) {
            $self->ParseMP4Container(
                $atom->node,
                $posit + $atom->offset,
                $posit + $atom->size - $atom->offset
            );
        }
        else {
            print( "done with noncontainer atom of atom of type ",
                $atom->type, "\n" )
              if $self->{DEBUG};
        }
        $posit += $atom->size;
    }
}

sub ParseStsd {
    my ( $self, $stsd ) = @_;
    $self->ParseMP4Container(
        $stsd->node,
        $stsd->start + 16,
        $stsd->start + $stsd->size - 16
    );
}

sub ParseDrms {
    my ( $self, $drms ) = @_;
    $self->ParseMP4Container(
        $drms->node,
        $drms->start + 36,
        $drms->start + $drms->size - 36
    );
    $self->{userID} = unpack 'N*', $self->FindAtomData('user');
    my $key = $self->FindAtomData('key');
    $self->{keyID}  = unpack 'N*', $key   if $key;
    $self->{priv} = $self->FindAtomData('priv');
    my $name = $self->FindAtomData('name');
    $self->{name} = substr( $name, 0, index( $name, "\0" ) );
    $self->{iviv} = $self->FindAtomData('iviv');
}

sub ParseMeta {
    my ( $self, $meta ) = @_;
    $self->ParseMP4Container(
        $meta->node,
        $meta->start + 12,
        $meta->start + $meta->size - 12
    );
    foreach my $type ( keys %meta_info_types ) {
        my $atom = $self->FindAtom($type) or next;
        my $adata =
          substr( $self->{buffer}, $atom->start + 24, $atom->size - 24 );
        next if length $adata > 300;
        if ( $type eq 'disk' ) {
            my ( $field1, $field2 ) = unpack "nn",
              substr( $self->{buffer}, $atom->start + $atom->size - 4, 4 );
            $adata = "Disk $field1 of $field2";
        }
        elsif ( $type eq 'trkn' ) {
            my ( $fld1, $fld2 ) = unpack "nn",
              substr( $self->{buffer}, $atom->start + $atom->size - 6, 4 );
            $adata = "Track $fld1 of $fld2";
        }
        elsif ( $type eq 'genre' ) {
            $adata = unpack 'n',
              substr( $self->{buffer}, $atom->start + $atom->size - 2, 2 );
        }
        $self->{meta}->{$type} = $adata unless length $adata > 300;
    }
    print $self->MetaInfo() if $self->{DEBUG};
}

sub AtomList {
    my ($self) = @_;
    return $self->{root}->getAllRelatives();    # returns ref to list of atoms
}

sub FindAtom {
    my ( $self, $type ) = @_;
    my @atoms = grep 
      { $type and $_->type and $_->type =~ /$type$/i } 
      @{ $self->AtomList() };
    return @atoms if wantarray;
    return unless scalar @atoms > 0;
    return $atoms[0];
}

sub FindAtomData {
    my ( $self, $type ) = @_;
    my $a = $self->FindAtom($type) or return;
    return $a->data;
}

sub MetaInfo {
    my ($self) = @_;
    my $meta_info = '';
    my $file_type = $self->GetFtype();
    $meta_info = "File type is $file_type\n" if $file_type;
    while ( my ( $mtype, $mdata ) = each %{ $self->{meta} } ) {
        $meta_info .= "Meta type $mtype, meta data $mdata\n";
    }
    return $meta_info;
}

sub AtomTree {
    my ($self) = @_;
    return $self->{root}->AtomTree();
}

sub DumpTree {
    my ( $self, $outfile ) = @_;
    if ( $outfile and open( my $dumpfh, ">$outfile" ) ) {
        print $dumpfh $self->AtomTree();
        close $dumpfh;
    }
    else { print $self->AtomTree() }
}

sub ConvertDrmsToMp4a {
    my ($self) = @_;
    my $diff = 0;
    my $drms = $self->FindAtom('drms') or return;
    foreach my $a (@m4p_not_m4a_atom_types) {
        my @unwanted = $self->FindAtom($a) or next;
        foreach my $u (@unwanted) { $diff += $u->size; $u->selfDelete() }
    }
    print "Shrunk file by $diff bytes during conversion\n" if $self->{DEBUG};
    $self->FixStco( $diff, $drms->start );
    $drms->type('mp4a');
}

sub FixStco {
    my ( $self, $sinf_sz, $change_position ) = @_;
    my @stco_atoms = $self->FindAtom('stco');
    my @co64_atoms = $self->FindAtom('co64');
    my @tfhd_atoms = $self->FindAtom('tfhd');
    # all Quicktime files should have at least one stco or co64 atom
    croak 'No stco or co64 atom' unless @stco_atoms || @co64_atoms;
    
    # if mdat is before change postion will not need to do anything
    my @mdat = $self->FindAtom('mdat') or return;
    my $all_mdat_before = 1;
    foreach my $mdt (@mdat) {
        $all_mdat_before = 0 if $mdt->start > $change_position;
    }
    return if $all_mdat_before;
    
    foreach my $stco (@stco_atoms) {
        my @samples =
          map { ($_ > $change_position) ? $_ - $sinf_sz : $_ }
          unpack( "N*",
            substr( $self->{buffer}, $stco->start + 16, $stco->size - 16 ) );
        substr(
            $self->{buffer},
            $stco->start + 16,
            $stco->size - 16,
            pack( 'N*', @samples )
        );
    }
    foreach my $co64 (@co64_atoms) {
        my @samples =
          unpack( "N*",
            substr( $self->{buffer}, $co64->start + 16, $co64->size - 16 ) );
        my $num_longs = scalar @samples;
        for ( my $i = 0 ; $i < $num_longs ; $i += 2 ) {
            my $high32bits = $samples[ $i ];
            my $low32bits = $samples[ $i + 1 ];
            my $offset64 = ( $high32bits * ( 2**32 ) ) + $low32bits;
            $offset64 -= $sinf_sz if $offset64 > $change_position;
            $samples[$i + 1] = $offset64 % (2**32);
            $samples[ $i ] = int($offset64 / (2**32) + 0.0001);
        }
        substr(
            $self->{buffer},
            $co64->start + 16,
            $co64->size - 16,
            pack( 'N*', @samples )
        );
    }
    foreach my $tfhd (@tfhd_atoms) {
        my($tf_flags, undef, $offset_high32, $offset_low32) =
          unpack('NNNN', substr( $self->{buffer}, $tfhd->start + 8, 16 ) );
        my $offset64 = ( $offset_high32 * ( 2**32 ) ) + $offset_low32;
        # we only need to adjust if the 1st movie fragment tf_flags bit is set
        next unless( ($tf_flags % 2) == 1 ); 
        next if $offset64 < $change_position;
        $offset64 -= $sinf_sz;
        $offset_high32 = int( $offset64 / (2**32) + 0.0001 );
        $offset_low32  = $offset64 % (2**32);
        substr( 
            $self->{buffer}, 
            $tfhd->start + 16,
            8,
            pack( 'NN', $offset_high32, $offset_low32 )
        );
    }    
}

sub GetSampleTable {
    my ($self) = @_;
    my $stsz = $self->FindAtom('stsz') or croak "No stsz table found";
    my $sampleCount = unpack 'N',
      substr( $self->{buffer}, $stsz->start + 16, 4 );
    my @samples = unpack 'N*',
      substr( $self->{buffer}, $stsz->start + 20, $sampleCount * 4 );
    print "There are $sampleCount samples in stsz atom.\n" if $self->{DEBUG};
    return \@samples;
}

sub DeleteAtom {
    my ( $self, $unwanted ) = @_;
    my $atom = $self->FindAtom($unwanted) or return;
    return $atom->selfDelete();
}

sub DeleteAtomWithStcoFix {
    my ( $self, $unwanted ) = @_;
    my $atom = $self->FindAtom($unwanted) or return;
    my $siz = $atom->size;
    my $pos = $atom->start;
    $atom->selfDelete() or return;
    $self->FixStco( $siz, $pos );
    return 1;
}

sub GetFtype {
    my($self) = @_;
    my $atom = $self->FindAtom('ftyp') or return;
    my $ftyp = substr($atom->data, 0, 4);
    $ftyp =~ s/^(\S+)\s+$/$1/;
    return $ftyp;
}

sub Get3GPInfo {
    my($self) = @_;
    while( my($meta_type, $atom_type) = each %asset_3GP_types ) {
        my $atom = $self->FindAtom($atom_type) or next;
        my $data = substr($atom->{buffer},$atom->start + 14,$atom->size - 14);
        $self->{MP4Info}->{$meta_type} = $data;
    }
    while ( my ( $tag, $alt_tag ) = each %alternate_tag_types ) {
        $self->{MP4Info}->{$alt_tag} = $self->{MP4Info}->{$tag}
          if exists $self->{MP4Info}->{$tag};
    }  
    my $file_type = $self->GetFtype();
    $self->{MP4Info}->{FTYP} = $file_type if $file_type;
    return $self->{MP4Info};
}

sub Set3GPInfo {
    my( $self, $field, $value, $delete_old ) = @_;
    my $asset_type = $asset_3GP_types{$field};
    my $moov = $self->FindAtom('moov') or croak "No moov atom found";
    my($asset, $udta);
    foreach my $typ (values %asset_3GP_types ) {
        $asset = $self->FindAtom($typ) or next;
        $udta = $asset->GetParent();
        last if $udta and $udta->{type} =~ /udta/i;
        $udta = 0;
    }
    # if cannot find any asset atoms, look for the udta child of moov
    $udta = $moov->DirectChildren('udta') unless $udta;
    # if no direct child of moov udta, make one
    unless($udta) {
        $moov->insertNew( 'udta', '' );
        $self->FixStco( -8, $moov->start );
        $udta = $moov->Contained('udta');
    }
    my $entry = $udta->Contained($asset_type);
    my $diff  = 0;
    if ( $entry and $delete_old ) {
        my @unwanted = $udta->Contained($asset_type);
        foreach my $u (@unwanted) {
            $diff += $u->size;
            $u->selfDelete;
        }
    }
    # now we can add the data
    # we set language code to 'eng'
    my $lang = 'eng';
    my $packed_lang = asset_language_pack_iso_639_2T($lang);
    my $data_packet = pack('Nn', 0,  $packed_lang) . $value;
    my $new_atom = $udta->insertNew( $asset_type, $data_packet );
    $diff -= $new_atom->size;
    $self->FixStco( $diff, $udta->start );
    return $new_atom;
}        

sub GetMetaInfo {
    my ( $self, $as_text ) = @_;
    
    # if we have a 3gp file, dispatch
    return $self->Get3GPInfo() if $self->GetFtype() =~ /^3g/;
    
    my %meta_tags;
    while ( my ( $meta_tag, $type ) = each %tag_types ) {
        $type =~ s/\W//g;
        my $atm      = $self->FindAtom($type)  or next;
        my $data_atm = $atm->Contained('data') or next;
        my $data     = $data_atm->data;
        if ( $type eq 'gnre' ) {
            ( undef, undef, $data ) = unpack 'NNn', $data;
        }
        elsif ( $type eq 'trkn' ) {
            ( undef, undef, undef, $data, $self->{MP4Info}->{TRACKCOUNT} ) =
              unpack 'NNnnn', $data;
        }
        else {
            my $firstchar = unpack( 'C', $data );
            $data = substr( $data, 8 ) unless $firstchar > 0;
        }
        $self->{MP4Info}->{$meta_tag} = $data;
        while ( my ( $tag, $alt_tag ) = each %alternate_tag_types ) {
            $self->{MP4Info}->{$alt_tag} = $self->{MP4Info}->{$tag}
              if defined $self->{MP4Info}->{$tag};
        }
    }
    if ($as_text) {

        # if as_text, we need to convert the tags to text
        if ( defined $self->{MP4Info}->{DISK} ) {
            ( undef, my $disknum, my $disks ) = unpack 'nnn',
              $self->{MP4Info}->{DISK};
            $self->{MP4Info}->{DISK} = "Disk $disknum of $disks";
        }
        if ( defined $self->{MP4Info}->{TRKN} ) {
            ( undef, my $tracknum, my $tracks ) = unpack 'nnn',
              $self->{MP4Info}->{TRKN};
            $self->{MP4Info}->{TRKN} = "Track $tracknum of $tracks"
              if $tracknum && $tracks;
        }
        if ( defined $self->{MP4Info}->{TMPO} ) {
            my $tempo = unpack 'n', $self->{MP4Info}->{TMPO};
            $self->{MP4Info}->{TMPO} = $tempo || "Undefined";
        }
        if ( defined $self->{MP4Info}->{CPRT} ) {
            $self->{MP4Info}->{CPRT} = substr( $self->{MP4Info}->{CPRT}, 3 );
        }
        if ( defined $self->{MP4Info}->{COVR} ) {
            $self->{MP4Info}->{COVR} = "Coverart present";
        }
        if (    defined $self->{MP4Info}->{GENRE}
            and $self->{MP4Info}->{GENRE} =~ /^\d+$/
            and $self->{MP4Info}->{GENRE} < 128 )
        {
            $self->{MP4Info}->{GENRE} =
              $genre_strings[ $self->{MP4Info}->{GENRE} - 1 ];
        }
    }
    $self->{MP4Info}->{FTYP} = $self->GetFtype();
    return $self->{MP4Info};
}

sub GetMP4Info {
    my ($self) = @_;
    my $meta = $self->GetMetaInfo();
    $meta->{LAYER}     = 1;
    $meta->{VERSION}   = 4;
    $meta->{COPYRIGHT} = ( exists $meta->{CPRT} ) ? 1 : 0;
    my $mdat = $self->FindAtom('mdat');
    $meta->{SIZE} = $mdat->size || 1;
    $meta->{ENCRYPTED} = $self->FindAtom('drms');
    my $mvhd_data = $self->FindAtomData('mvhd');
    my ( $timescale, $duration );

    if ($mvhd_data) {
        my @mvhd = unpack( 'Ca3NNNNNN', $mvhd_data );
        if ( $mvhd[0] == 1 ) {
            $timescale = $mvhd[6];
            $duration = ( $mvhd[7] * ( 2**32 ) + $mvhd[8] ) / $mvhd[6];
        }
        else {
            $timescale = $mvhd[4];
            $duration  = $mvhd[5];
        }
        $meta->{SECONDS} = int( $duration / $timescale + 0.5 );
        $meta->{MM}      = int( $meta->{SECONDS} / 60 );
        $meta->{SS}      = $meta->{SECONDS} % 60;
        $meta->{BITRATE} = int( $meta->{SIZE} / $meta->{SECONDS} + 0.5 );
    }
    return $meta;
}

sub SetMetaInfo {
    my ( $self, $field, $value, $delete_old, $before, $as_text ) = @_;
    $self->GetMetaInfo;    # fill default fields like TRACKCOUNT
    
    # if we have a 3gp file, dispatch
    return $self->Set3GPInfo( $self, $field, $value, $delete_old ) 
      if $self->GetFtype() =~ /^3g/;;
    
    my $type = $tag_types{$field} || lc substr( $field, 0, 4 );
    my $ilst = $self->FindAtom('ilst') || $self->MakeIlstAtom || return;
    my $typ = $type;
    $typ =~ s/\W//g;
    my $entry = $ilst->Contained($typ);
    my $diff  = 0;
    if ( $entry and $delete_old ) {
        my @unwanted = $ilst->Contained($typ);
        foreach my $u (@unwanted) {
            $diff += $u->size;
            $u->selfDelete;
        }
    }
    if ($as_text) {
        my %iTMS_meta_atoms = reverse %iTMS_dict_meta_types;
        if ( $iTMS_meta_atoms{$type} ) {
            my %h;
            if ( $type eq 'disk' and $value =~ m/(\d+)\D+(\d+)/ ) {
                $h{discNumber} = $1;
                $h{discCount}  = $2;
            }
            elsif ( $type eq 'trkn' and $value =~ m/(\d+)\D+(\d+)/ ) {
                $h{trackNumber} = $1;
                $h{trackCount}  = $2;
            }
            elsif ( $type eq 'gnre' ) {
                $value = genre_text_to_genre_num($value);
            }
            else { $h{ $iTMS_meta_atoms{$type} } = $value }
            $self->FixStco( $diff, $ilst->start ) if $diff;
            return $self->iTMS_MetaInfo( \%h, 1 );
        }
    }
    if ( $typ eq 'covr' and $ilst->Contained($typ) ) {
        $ilst->addMoreArtwork($value);
        $diff -= 16;
    }
    else {
        $ilst->insertNewMetaData( $type, $value, $before );
        $diff -= 24;
    }
    $diff -= length $value;
    $self->FixStco( $diff, $ilst->start );
}


sub MakeIlstAtom {
    my ($self) = @_;
    my $moov = $self->FindAtom('moov') or croak "No moov atom found";
    my $udta = $moov->DirectChildren('udta');
  
    # if no udta under moov, make one under moov
    unless($udta) {
        $moov->insertNew( 'udta', '' );
        $self->FixStco( -8, $moov->start );
        $udta = $moov->Contained('udta');
    }
    
    # if we have a meta atom, add an hdlr atom and ilst to it 
    # if we do not have one make one 
    my( $meta, $hdlr, $ilst );
    $meta = $udta->Contained('meta');
    unless($meta) {
        $udta->insertNew( 'meta', "\0\0\0\0" );
        $self->FixStco( -12, $udta->start );
        $meta = $udta->Contained('meta');
    }
    $hdlr = $meta->Contained('hdlr');
    unless($hdlr) {
        $meta->insertNew( 'hdlr', "\0\0\0\0\0\0\0\0mdirappl\0\0\0\0\0\0\0\0\0" );
        $self->FixStco( -33, $meta->start );
        $hdlr = $meta->Contained('hdlr');
    }
    $ilst = $meta->Contained('ilst');
    unless($ilst) {
        $meta->insertNew( 'ilst', '' );
        $self->FixStco( -8, $meta->start );
        $ilst = $meta->Contained('ilst');
    }
    return $ilst;
}

sub iTMS_MetaInfo {
    my ( $self, $dict, $keep_old ) = @_;
    my ( $key, $type, %info );
    if ($dict) {
        while ( ( $key, $type ) = each %iTMS_dict_meta_types ) {
            next if $key =~ /Count$/;
            next unless exists $dict->{$key};
            my $data = $dict->{$key};
            if ( $key eq 'discNumber' ) {
                my $count = $dict->{discCount} or next;
                $data = pack "nnn", 0, $data, $count;
            }
            if ( $key eq 'trackNumber' ) {
                my $count = $dict->{trackCount} or next;
                $data = pack "nnnn", 0, $data, $count, 0;
            }
            if ( $key eq 'artworkURL' ) {
                eval 'require LWP::Simple; $data = get($data)';
            }
            if ( $key eq 'copyright' ) {
                $data = "\xE2\x84\x97 " . $data;
            }
            if ( $key eq 'genre' ) {
                my $gnre = genre_text_to_genre_num($data);
                $data = pack "n", $gnre unless $gnre eq 'INVALID_GENRE';
            }
            $self->SetMetaInfo( $type, $data, $keep_old ? undef : 1, undef, undef );
        }
    }
    while ( ( $key, $type ) = each %iTMS_dict_meta_types ) {
        my $meta = $self->FindAtom($type) or next;
        my $data = substr( $meta->data, 16 );
        if ( $type eq 'trkn' ) {
            ( undef, $info{trackNumber}, $info{trackCount} ) = unpack "nnn",
              $data;
        }
        elsif ( $type eq 'disk' ) {
            ( undef, $info{diskNumber}, $info{discCount} ) = unpack "nnn",
              $data;
        }
        elsif ( $type eq 'cprt' ) {
            ( undef, $info{'copyright'} ) = split( /\s+/, $data, 2 );
        }
        else { $info{$key} = $data }
    }
    return \%info;
}

# Get cover art--returns a reference to an array of cover artwork
sub GetCoverArt {
    my ($self) = @_;
    my @covr = $self->FindAtom('covr') or return;
    my @artwork;
    foreach my $atm (@covr) {
        my $data_atm = $atm->Contained('data') or next;
        push @artwork, substr( $data_atm->data, 8 );
    }
    return \@artwork;
}

#-----------------------------------------------------------

# MP3::Tag analogs, but more fields, and allow setting of tags

sub autoinfo {
    my ($self) = @_;
    my $tags = $self->GetMetaInfo;
    return (
        $tags->{TITLE},   $tags->{TRKN}, $tags->{ARTIST}, $tags->{ALBUM},
        $tags->{COMMENT}, $tags->{YEAR}, $tags->{GENRE}
    );
}

sub title {
    my ( $self, $new_tag ) = @_;
    $self->SetMetaInfo( 'TITLE', $new_tag, 1 ) if $new_tag;
    my $tags = $self->GetMetaInfo;
    return $tags->{TITLE} || '';
}

sub album {
    my ( $self, $new_tag ) = @_;
    $self->SetMetaInfo( 'ALBUM', $new_tag, 1 ) if $new_tag;
    my $tags = $self->GetMetaInfo;
    return $tags->{ALBUM} || '';
}

sub artist {
    my ( $self, $new_tag ) = @_;
    $self->SetMetaInfo( 'ARTIST', $new_tag, 1 ) if $new_tag;
    my $tags = $self->GetMetaInfo;
    return $tags->{ARTIST} || '';
}

sub comment {
    my ( $self, $new_tag ) = @_;
    $self->SetMetaInfo( 'COMMENT', $new_tag, 1 ) if $new_tag;
    my $tags = $self->GetMetaInfo;
    return $tags->{COMMENT} || '';
}

sub year {
    my ( $self, $new_tag ) = @_;
    $self->SetMetaInfo( 'YEAR', $new_tag, 1 ) if $new_tag;
    my $tags = $self->GetMetaInfo;
    return $tags->{YEAR} || 0;
}

sub genre {
    my ( $self, $new_tag ) = @_;
    $self->SetMetaInfo( 'GENRE', pack( "n", $new_tag ), 1 ) if $new_tag;
    my $tags = $self->GetMetaInfo;
    return $tags->{GENRE} || '';
}

sub genre_as_text {
    my ( $self, $new_tag ) = @_;
    my ( $i, $genre_num );
    if ($new_tag) {
        $self->genre( genre_text_to_genre_num($new_tag) );
    }
    return genre_num_to_genre_text( $self->genre );
}

sub track {
    my ( $self, $new_trkn ) = @_;
    my $tags = $self->GetMetaInfo(1);
    if ($new_trkn) {
        my $tcount = $tags->{TRACKCOUNT} || 0;
        $self->SetMetaInfo( 'TRKN', "$new_trkn of $tcount", 1, 0, 1 );
        $tags = $self->GetMetaInfo(1);
    }
    return $tags->{TRKN} || 0;
}

sub tracks {
    my ( $self, $new_trkn, $new_tcount ) = @_;
    $self->SetMetaInfo( 'TRKN', "$new_trkn of $new_tcount", 1, 0, 1 )
      if ( $new_trkn and $new_tcount );
    my $tags   = $self->GetMetaInfo;
    my $trkn   = $tags->{TRKN} || 0;
    my $tcount = $tags->{TRACKCOUNT} || 0;
    return ( $trkn, $tcount );
}

sub total {
    my ( $self, $new_tcount ) = @_;
    my $tags;
    if ($new_tcount) {
        $tags = $self->GetMetaInfo;
        $self->SetMetaInfo( 'TRKN', $tags->{TRKN} . " of $new_tcount", 1, 0,
            1 );
    }
    $tags = $self->GetMetaInfo;
    my $trkn   = $tags->{TRKN}       || 0;
    my $tcount = $tags->{TRACKCOUNT} || 0;
    return $tcount;
}

sub all_tags {
    my ( $self, $tags_href ) = @_;
    if ( ref $tags_href ) {
        $self->title( $tags_href->{title} )     if $tags_href->{title};
        $self->artist( $tags_href->{artist} )   if $tags_href->{artist};
        $self->album( $tags_href->{album} )     if $tags_href->{album};
        $self->comment( $tags_href->{comment} ) if $tags_href->{comment};
        $self->genre( $tags_href->{genre} )     if $tags_href->{genre};
        $self->year( $tags_href->{year} )       if $tags_href->{year};
        $self->track( $tags_href->{track} )     if $tags_href->{track};
        $self->total( $tags_href->{total} )     if $tags_href->{total};
    }
    return {
        title   => $self->title(),
        artist  => $self->artist(),
        album   => $self->album(),
        comment => $self->comment(),
        genre   => $self->genre(),
        year    => $self->year(),
        track   => $self->track(),
        total   => $self->total(),
    };
}

#------------ other compatibility functions with Audio::TagLib -----------------#

sub setTitle   { title(@_) }
sub setArtist  { artist(@_) }
sub setAlbum   { album(@_) }
sub setComment { comment(@_) }
sub setGenre   { genre(@_) }
sub setTrack   { track(@_) }
sub setTracks  { tracks(@_) }
sub setTotal   { total(@_) }

#-------------- non-self helper functions --------------------------#

sub isMetaDataType {
    return $meta_info_types{shift};
}

sub genre_text_to_genre_num {
    my $text = shift;
    my $genre_num;
    if ($text) {
        my $i = 1;
        foreach my $t (@genre_strings) {
            if ( $t eq $text ) {
                $genre_num = $i;
                last;
            }
            ++$i;
        }
    }
    return $genre_num;
}

sub genre_num_to_genre_text {
    my $num = shift;
    return unless $num > 0 and $num <= scalar @genre_strings;
    return $genre_strings[ $num - 1 ];
}

sub asset_language_pack_iso_639_2T {
    my($lang3chars) = @_;
    my( $c1, $c2, $c3 ) = map { $_ ? ord($_) - 60 : 0 } split (//, $lang3chars);
    return ($c1 * (2 ** 10)) + ($c2 * (2 ** 5)) + $c3;
}


=head1 NAME

Audio::M4P::QuickTime -- Perl module for m4p/mp4/m4a Quicktime audio files

=head1 DESCRIPTION

Perl manipulation of Quicktime Audio files, including protected audio M4P 
files. Allows extraction and modification of meta information in Apple 
QuickTime AAC/m4a music files.

=head2 About QuickTime File Structure and Atoms

M4P is a QuickTime protected audio file format. It is composed of a linear
stream of bytes which are segmented into units called atoms. Some atoms 
may be containers for other atoms. iTunes Music Store M4P music files are 
Quicktime audio files which are encrypted using a combination of information 
in the file's drms atom and information which is commonly stored on the 
computer or audio player. 

=head1 SYNOPSIS

=over 4

 use Audio::M4P::QuickTime;

 my $mp4file = "file.m4p";

 my $qt = new Audio::M4P::QuickTime(file => $mp4file);

 my $tags = $qt->GetMetaInfo;

 print "Artist is $tags->{ARTIST}\n" if $tags->{ARTIST};

=back

=head1 METHODS

=head2 Object Methods

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
DEBUG => 2, DEBUGDUMPFILE => "file" causes an HTML tree representation 
of the QuickTime file to be emitted to the file given as value to the 
argument pair. file => "filename.m4p" causes the named QuickTime file to 
be read and parsed during object initialization.

=item B<ReadFile>

 $qt->ReadFile("filename.m4a");

Read the named file into the QuickTime object buffer.

=item B<ParseBuffer>

 $qt->ParseBuffer;

Parse the file that has been read as a QuickTime stream.

=item B<WriteFile>

 $qt->WriteFile("ouput.m4p");

Write the (possibly modified) file back to the output file argument.

=item B<GetMetaInfo>

 my $hashref = $qt->GetMetaInfo(1);
 while(my($tag, $value) = each %{$hashref}) { 
    print "$tag => $value\n";
 }

Returns a hash reference to meta tag information. Attempts to be compatible 
with tag information formats in MP3::Info and MP4::Info. Potential tags are 
AAID, ALBUM, ARTIST, COMMENT, COM, CPIL, CPRT, YEAR, DISK, GENRE, GRP, NAM,  
RTNG, TMPO, TOO, TRKN, and WRT. Note that, due to preservation of compatibility 
with MP3::Info by returning tag info as a hash reference, duplicate entries of 
the same tag name, such as multiple comment fields, will not be returned in the hash 
reference. An optional second argument, if 1 or true, should convert some 
binary fields to text in the tags, for instance 
 my $hashref = $qt->GetMetaInfo(1);

=item B<GetMP4Info>

 my $hashref = $qt->GetMP4Info;
 while(my($tag, $value) = each %{$hashref}) { 
    print "$tag => $value\n";
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
to be placed in the file. The fifth argument indicates the values are in text 
form, ie for meta type 'trkn', value is something like 'Track 5 of 11'.

=item B<iTMS_MetaInfo>

 my $hashref = $qt->iTMS_MetaInfo;
 
 $hashref->{comments} = "A new comment";
 $qt->iTMS_MetaInfo($hashref);
 
Get or set a meta information field via a hash reference to an Apple iTMS
type dict data structure. Possible fields are copyright, comments, 
songName, genre, playlistArtistName, genreID, composerName, playlistName,
year, trackNumber, trackCount, discNumber, discCount, and artworkURL. iTMS 
meta data entries may not be compatible with MP3::Info type meta data. An
optional second argument, if true, prevents the method from replacing old meta
information, as in $qt->iTMS_MetaInfo($hashref, 1);

Note that although this method of manipulating M4P data tags is closest to the 
way iTMS and iTunes do metadata, it may be less intuitive for most audio tag 
programmers than the MP3::Tag and Audio::TagLib compatible methods below.

=item B<GetCoverArt>

  my $artwork = $qt->GetCoverArt();
  foreach my $pic (@{$artwork}) { 
      # do stuff with art
  }
  
Returns a reference to an array of cover artwork. Note: the artwork routines
were suggested and largely contributed by pucklock. (Thanks!)

=back

=head2 MP3::Tag and Audio::TagLib Compatible Functions

=over 4

=item B<autoinfo>

  my($title, $tracknum, $artist, $album, $comment, $year, $genre) =
    $qt->autoinfo;

Returns an array of tag metadata, similar to the same method in MP3::Tag.

=item B<album>

  my $album = $qt->album;
  $new_album = "My New Album Name";
  $qt->album($new_album);

Get and set title tag data.
Similar to the same method in MP3::TagLib.

Note this and other tag functions below will usually return the empty 
string "" when there is tag data lacking, unless an integer result is expected, 
in which case 0 is returned. This is for compatibility with MP3::Tag and 
Audio::TagLib's implementation of these methods.

=item B<artist>

  my $artist = $qt->artist;
  $new_artist = "My New Artist";
  $qt->artist($new_artist);

Get and set artist tag data.
Similar to the same method in MP3::TagLib.

=item B<comment>

  my $comment = $qt->comment;
  $new_comment = "My Comment Goes Here";
  $qt->comment($new_comment);

Get and set comment tag data.
Similar to the same method in MP3::Tag.

=item B<genre>

  my $genre = $qt->genre;
  $new_genre = 18;
  $qt->genre($new_genre);

Get and set genre tag data BY NUMBER.

=item B<genre_as_text>

  my $text_genre = $qt->genre_as_text;
  $new_genre = "Rock";
  $qt->genre_as_text($new_genre);

Get and set genre tag data as text. Note that the given text tag must exist 
in the genre database to work. See the "our @genre_strings" object in the 
code, which can be imported by the declaration "our @genre_strings;" 
in code using the module.

=item B<title>

  my $title = $qt->title;
  $new_title = "My New One";
  $qt->title($new_title);

Get and set title tag data.
Similar to the same method in MP3::Tag.

=item B<track>

  my $track = $qt->track;
  my $new_track = 3;
  $qt->track($new_track);

Get or set the track number.

=item B<tracks>

  my ($track, $count) = $qt->tracks;
  my $new_track_number = 3;
  my $total_tracks_on_CD = 17;
  $qt->tracks($new_track_number, $total_tracks_on_CD);

Get or set both the track number and the total tracks on the originating media 
work. Not actually an MP3::Tag method, but MP4 files, unlike many MP3 files, 
regularly contain both track number and the total originating CD's track count.

=item B<total>

  my $total = $qt->total;
  my $new_total = 15;
  $qt->total($new_total);

Get or set the track total number.

=item B<year>

  my $year = $qt->year;
  $new_year = "My New One";
  $qt->year($new_year);

Get and set year tag data.
Similar to the same method in MP3::Tag.

=item B<all_tags>

  my $tref = $qt->all_tags( album => "My new album", genre => 21 );
  print $tref->{artist};

Similar to the Audio::File::Tag B<all> method. Set or get all the above tags. 
To set the tags pass a hash reference with the names of the tags as keys and 
the tag values as hash values. Returns a hash reference if no argument is 
specified.

The following tag names are supported by this method:
album
artist
comment
genre   ( the integer value genre )
title
track
total

=back

=head2 Other Audio::TagLib syntactic compatibility 

=over 4

=item The following 'set' methods are equivalent to methods above used with an argument. They are included in this module for Audio::TagLib compatibility:

=item Method     equivalent to

=item ------------------------

=item setAlbum     album

=item setArtist    artist

=item setTitle     title

=item setComment   comment

=item setGenre     genre

=item setTrack     track

=item setTracks    tracks

=item setTotal     total tracks

=back

=head2 Class Internal Methods and Functions

=over 4

=item AtomList

=item AtomTree

=item ConvertDrmsToMp4a

=item DeleteAtom

=item DeleteAtomWithStcoFix

=item DumpTree

=item FindAtom

=item FindAtomData

=item FixStco

=item GetSampleTable

=item MakeIlstAtom

=item MetaInfo

=item ParseDrms

=item ParseMP4Container

=item ParseMeta

=item ParseStsd

=item genre_num_to_genre_text

=item genre_text_to_genre_num

=item isMetaDataType

=item Get3GPInfo

=item GetFtype

=item Set3GPInfo

=item asset_language_pack_iso_639_2T

=back

=head1 BUGS

=over 4

The Audio::M4P::* code is not re-entrant on a per-file basis, due to recursive 
changes to containers not being thread-safe. Threaded code using these modules 
may need to lock down all method calls with a semaphore or other serialization 
method, unless only one thread is used to modify any given audio file.

=back

=head1 SEE ALSO WITH THIS MODULE

=over 4

=item L<Audio::M4P>, L<Audio::M4P::Atom>, L<Audio::M4PDecrypt>

=item L<LWP::UserAgent::iTMS_Client>

=back    

=head1 SEE ALSO

=over 4

=item L<MP3::Info>, L<MP4::Info>, L<MP3::Tag>, L<Audio::TagLib>, L<Audio::File::Tag>, L<Mac::iTunes>, L<Net::iTMS>, L<LWP::UserAgent::iTMS_Client>

=back

=head1 AUTHOR 

=over 4

William Herrera B<wherrera@skylightview.com>. 

=back

=head1 SUPPORT 

=over 4

Questions, feature requests and bug reports should go to 
<wherrera@skylightview.com>.

=back

=head1 COPYRIGHT 

=over 4

Copyright (c) 2003-2005 William Herrera. All rights reserved.  
This program is free software; you can redistribute it and/or modify 
it under the same terms as Perl itself.

=back

=cut

1;
