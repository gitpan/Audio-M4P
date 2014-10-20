package Audio::M4P::Atom;

require 5.006;
use strict;
use warnings;
use Carp;
our $VERSION = '0.53';

use Scalar::Util 'weaken';

use Tree::Simple 'use_weak_refs';
use Tree::Simple::Visitor;
use Tree::Simple::View::HTML;

# see http://www.geocities.com/xhelmboyx/quicktime/formats/mp4-layout.txt
my %container_atom_types = (
    aaid   => 1,
    akid   => 1,
    '�alb' => 1,
    apid   => 1,
    aART   => 1,
    '�ART' => 1,
    atid   => 1,
    clip   => 1,
    '�cmt' => 1,
    '�com' => 1,
    covr   => 1,
    cpil   => 1,
    cprt   => 1,
    '�day' => 1,
    dinf   => 1,
    disk   => 1,
    drms   => 1,
    edts   => 1,
    geid   => 1,
    gnre   => 1,
    '�grp' => 1,
    hinf   => 1,
    hnti   => 1,
    ilst   => 1,
    matt   => 1,
    mdia   => 1,
    meta   => 1,
    minf   => 1,
    moof   => 1,
    moov   => 1,
    mp4a   => 1,
    '�nam' => 1,
    pinf   => 1,
    plid   => 1,
    rtng   => 1,
    schi   => 1,
    sinf   => 1,
    stbl   => 1,
    stik   => 1,
    stsd   => 1,
    tmpo   => 1,
    '�too' => 1,
    traf   => 1,
    trak   => 1,
    trkn   => 1,
    udta   => 1,
    '�wrt' => 1,
);

my %noncontainer_atom_types = (
    chtb   => 1,
    ctts   => 1,
    data   => 1,
    esds   => 1,
    free   => 1,
    frma   => 1,
    ftyp   => 1,
    '�gen' => 1,
    hmhd   => 1,
    iviv   => 1,
    'key ' => 1,
    mdat   => 1,
    mdhd   => 1,
    mp4s   => 1,
    mpv4   => 1,
    mvhd   => 1,
    name   => 1,
    priv   => 1,
    rtp    => 1,
    sign   => 1,
    stco   => 1,
    stsc   => 1,
    stp    => 1,
    stts   => 1,
    tfhd   => 1,
    tkhd   => 1,
    tref   => 1,
    trun   => 1,
    user   => 1,
    vmhd   => 1,
    wide   => 1,
);

sub int64toN {
    my ($int64) = @_;
    my $high32bits = pack( 'N', int( $int64 / ( 2**32 ) + 0.0001 ) );
    my $low32bits = pack( 'N', $int64 % ( 2**32 ) );
    return $high32bits . $low32bits;
}

sub int64fromN {
    my ($buf) = @_;
    my ( $high32bits, $low32bits ) = unpack( "NN", $buf );
    return ( $high32bits * ( 2**32 ) ) + $low32bits;
}

# begin class methods

sub new {
    my ( $class, %args ) = @_;
    my $self = \%args;
    bless( $self, $class );
    $self->{node} = Tree::Simple->new($self);
    if( ref $self->{parent} ) {
        $self->{parent}->addChild( $self->{node} );
        weaken $self->{node};
        weaken $self->{parent};
    }
    else {     
        $self->{parent} = 0;
    }
    if( ref $self->{rbuf} ) {
        weaken $self->{rbuf};
        $self->read_buffer( $self->{read_buffer_position} )
          if exists $self->{read_buffer_position};
    }    
    return $self;
}

sub DESTROY {
    my($self) = @_;
    delete $self->{parent};
    delete $self->{rbuf};
    return unless ref $self->{node};
    my @kids = $self->{node}->getAllChildren();
    foreach my $child (@kids) {
        next unless ref $child;
        my $val = $child->getNodeValue();
        $val->DESTROY 
                    if ref $val 
                    and ref $val->{parent}
                    and $val->{parent} eq $self;
    }
    $self->{node}->DESTROY if ref $self->{node};
    delete $self->{node};
}

sub parent { return shift->{parent} }

sub node { return shift->{node} }

sub rbuf { return shift->{rbuf} }

sub read_buffer {
    my ( $self, $starting ) = @_;
    $self->{start}  = $starting;
    $self->{offset} = 8;
    ( $self->{size}, $self->{type} ) = unpack 'Na4',
      substr( ${ $self->{rbuf} }, $starting, 8 );
    if ( $self->{size} == 1 ) {
        $self->{size} =
          int64fromN( substr( ${ $self->{rbuf} }, $starting + 8, 8 ) );
        $self->{offset} = 16;
    }
    return $self->{size};
}

sub type {
    my ( $self, $newtype ) = @_;
    if ( defined $newtype ) {
        $self->{type} = substr( $newtype, 0, 4 );
        substr( ${ $self->{rbuf} }, $self->{start} + 4, 4, $self->{type} );
    }
    return $self->{type};
}

sub start {
    my ( $self, $newstart ) = @_;
    $self->{start} = $newstart if defined $newstart;
    return $self->{start};
}

sub size {
    my ( $self, $newsize ) = @_;
    if ( defined $newsize ) {
        return $self->BigResize($newsize)
          if $newsize >= 2**32
          and $self->{size} >= 2**32;
        return $self->toBigSize($newsize)
          if $newsize >= 2**32
          and $self->{size} < 2**32;
        return $self->toRegularSize($newsize)
          if $self->{size} >= 2**32
          and $newsize < 2**32;
        $self->{size} = $newsize;
        substr( ${ $self->{rbuf} }, $self->{start}, 4, pack( 'N', $newsize ) );
    }
    return $self->{size};
}

sub BigResize {
    my ( $self, $newsize ) = @_;
    croak "atom size big, but offset not 16" if $self->{offset} != 16;
    $self->{size} = $newsize;
    substr( ${ $self->{rbuf} }, $self->{start} + 8, 8, int64toN($newsize) );
    return $self->{size};
}

sub toBigSize {
    my ( $self, $newsize ) = @_;

    # need to add 2 bytes to the data section and reset containers and starts
    return unless $self->{offset} == 8 and $newsize >= 2**32;
    $self->{offset} = 16;
    $self->{size}   = $newsize;
    substr( ${ $self->{rbuf} }, $self->{start}, 4, pack( 'N', 1 ) );
    substr( ${ $self->{rbuf} }, $self->{start} + 8, 0, int64toN($newsize) );
    $self->redoStarts(8);
    $self->resizeContainers(8) unless $self->{type} eq 'moov';
    return $self->{size};
}

sub toRegularSize {
    my ( $self, $newsize ) = @_;

    # need to remove 2 bytes from data section and reset containers and starts
    return unless $self->{offset} == 16 and $newsize < 2**32;
    $self->{offset} = 8;
    $self->{size}   = $newsize;
    substr( ${ $self->{rbuf} }, $self->{start}, 4, pack( 'N', $newsize ) );
    substr( ${ $self->{rbuf} }, $self->{start} + 8, 8, '' );
    $self->redoStarts(-8);
    $self->resizeContainers(-8) unless $self->{type} eq 'moov';
    return $self->{size};
}

sub offset {
    my ( $self, $o ) = @_;
    $self->{offset} = $o if defined($o) and ( $o == 8 or $o == 16 );
    return $self->{offset};
}

sub data {
    my ( $self, $newdata ) = @_;
    if ( defined $newdata ) {
        my $newsize = ( length $newdata ) + 8;
        my $diff    = $newsize - $self->{size};
        $self->resizeContainers($diff);
        substr(
            ${ $self->{rbuf} },
            $self->{start} + $self->{offset},
            $self->{size} - $self->{offset}, $newdata
        );
        $self->size($newsize);
        $self->redoStarts( $diff, $self->{start} );
    }
    return substr(
        ${ $self->{rbuf} },
        $self->{start} + $self->{offset},
        $self->{size} - $self->{offset}
    );
}

sub root {
    my ($self) = @_;
    return $self->node if $self->node->isRoot();
    return unless ref $self->{parent};
    return $self->{parent}->getNodeValue()->root();
}

sub getAllRelatives {
    my ($self) = @_;
    my $visitor = Tree::Simple::Visitor->new();
    $self->root()->accept($visitor);
    my @a = $visitor->getResults;
    return \@a;
}

sub AtomTree {
    my ($self) = @_;
    my $view = Tree::Simple::View::HTML->new(
        $self->{node},
        (
            list_css       => "list-style: circle;",
            list_item_css  => "font-family: courier;",
            node_formatter => sub {
                my ($tree) = @_;
                return "<em> " . $tree->getNodeValue->print() . " </em>";
            },
        )
    );
    return $view->expandAll();
}

sub resizeContainers {
    my ( $self, $diff ) = @_;
    if ( $self->{parent} and ref $self->{parent} ) {
        my $container = $self->{parent}->getNodeValue();
        if ( $container->{type} ne 'file' ) {
            $container->size( $container->size + $diff );
            $container->resizeContainers($diff)
              unless $container->{type} eq 'moov';
        }
    }
}

sub redoStarts {
    my ( $self, $diff, $pivot ) = @_;
    foreach my $atom ( @{ $self->getAllRelatives() } ) {
        $atom->{start} += $diff
          if $atom->{start} >= $pivot
          and $atom != $self;
    }
}

sub selfDelete {
    my ($self) = @_;
    $self->resizeContainers( -$self->size );
    substr( ${ $self->{rbuf} }, $self->start, $self->size, '' );
    $self->redoStarts( -$self->size, $self->{start} );
    return unless ref $self->{parent};
    $self->{parent}->removeChild( $self->{node} );
    delete $self->{parent};
    return 1;
}

sub insertNew {
    my ( $self, $type, $data, $before ) = @_;
    my $node = $self->{node};
    my $atom = new Audio::M4P::Atom( parent => $node, rbuf => $self->{rbuf} );
    my $after_atom;
    if ( $before and ( $after_atom = $self->Contained($before) ) ) {
        $atom->{start} = $after_atom->{start};
    }
    else { $atom->{start} = $self->{start} + $self->{size}; }
    $atom->{offset} = 8;
    $atom->{size}   = 8 + length $data;
    $atom->{type}   = $type;
    $atom->redoStarts( $atom->{size}, $atom->{start} );
    my $buf = pack( 'Na4', $atom->{size}, $type ? $type : 'junk' ) . $data;
    substr( ${ $self->{rbuf} }, $atom->{start}, 0, $buf );
    $self->size( $self->{size} + $atom->{size} );
    $self->resizeContainers( $atom->{size} );
    return $atom;
}

sub insertNewMetaData {
    my ( $self, $type, $data, $before ) = @_;
    my $wrapper = $self->insertNew( $type, '', $before );
    my $flag =
        ( $type =~ /gnre|disk|trkn/i ) ? 0
      : ( $type =~ /rtng/i ) ? 21
      : ( $type =~ /covr/i ) ? 13
      : 1;
    $wrapper->insertNew( 'data', pack( 'NN', $flag, 0 ) . $data );
}

sub addMoreArtwork {

    # add more artwork to a covr atom contained in self
    my ( $self, $data ) = @_;
    my $covr = $self->Contained('covr') or croak "No covr atom in this atom";
    $covr->insertNew( 'data', pack( 'NN', 13, 0 ) . $data );
}

sub Container {
    my ( $self, $container_type ) = @_;
    return unless ref $self->{parent};
    my $parent_atom = $self->{parent}->getNodeValue();
    return $parent_atom if $parent_atom->{type} =~ /$container_type/i;
    return $parent_atom->Container($container_type);
}

sub Contained {
    my ( $self, $type ) = @_;
    my $node = $self->{node};
    my @kids = $node->getAllChildren();
    my @results;
    foreach my $child (@kids) {
        my $val = $child->getNodeValue();
        push @results, $val if $val->{type} and $val->{type} =~ /$type/i;
    }
    return @results if wantarray;
    return unless scalar @results > 0;
    return $results[0];
}

sub isContainer {
    my ($self) = @_;
    return $container_atom_types{ $self->{type} };
}

sub ParentAtom {
    my ($self) = @_;
    return unless ref $self->{parent};
    return $self->{parent}->getNodeValue();
}

sub DirectChildren {
    my ( $self, $type ) = @_;
    my @kids = $self->Contained($type);
    my @results;
    foreach my $a (@kids) {
        push @results, $a if $a->ParentAtom() eq $self;
    }
    return @results if wantarray;
    return unless scalar @results > 0;
    return $results[0];
}

sub print {
    my ($self) = @_;
    return "Atom "
      . $self->type . " at "
      . $self->start
      . " size "
      . $self->size
      . " ends at "
      . ( $self->start + $self->size );
}

=head1 NAME

Audio::M4P::Atom -- M4P/MP4/M4A QuickTime audio music format atoms

=head1 DESCRIPTION
    
M4P is a QuickTime protected audio file format. It is composed of a linear
stream of bytes which are segmented into units called atoms. Some atoms
may contain other atoms. This module has methods for handling atoms which 
are delegated by the QuickTime and other modules in the Audio::M4P hierarchy.
   
=head2 Class Internal Functions

=over 4

=item B<AtomTree>

=item B<BigResize>

=item B<Contained>

=item B<Container>

=item B<DirectChildren>

=item B<ParentAtom>

=item B<addMoreArtwork>

=item B<data>

=item B<getAllRelatives>

=item B<insertNew>

=item B<insertNewMetaData>

=item B<int64fromN>

=item B<int64toN>

=item B<isContainer>

=item B<new>

=item B<node>

=item B<offset>

=item B<parent>

=item B<print>

=item B<rbuf>

=item B<read_buffer>

=item B<redoStarts>

=item B<resizeContainers>

=item B<root>

=item B<selfDelete>

=item B<size>

=item B<start>

=item B<toBigSize>

=item B<toRegularSize>

=item B<type>

=back
   
=head1 AUTHOR 

    William Herrera B<wherrera@skylightview.com>. 

=head1 SUPPORT 

Questions, feature requests and bug reports should go to 
<wherrera@skylightview.com>.


=cut

1;
