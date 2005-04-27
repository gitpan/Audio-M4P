package Audio::M4P::Atom;

require 5.004;
use strict;
use warnings;
use Carp;
use vars qw($VERSION);
$VERSION = '0.08';

use Tree::Simple;
use Tree::Simple::Visitor;
use Tree::Simple::View::HTML;

# see http://www.geocities.com/xhelmboyx/quicktime/formats/mp4-layout.txt
my %container_atom_types = ( 
'©alb' => 1, '©ART' => 1,   clip => 1, '©cmt' => 1,   covr => 1,   cpil => 1,
'©day' => 1,   dinf => 1,   disk => 1,   drms => 1,   edts => 1,   ilst => 1, 
  matt => 1,   mdia => 1,   meta => 1,   minf => 1,   moov => 1, '©nam' => 1, 
  schi => 1,   sinf => 1,   stbl => 1,   tmpo => 1, '©too' => 1,   trak => 1,
  trkn => 1,   udta => 1, '©wrt' => 1,
);

my %noncontainer_atom_types = (
ctts => 1,   data => 1, esds => 1, free => 1, frma  => 1,    
ftyp => 1, '©gen' => 1, gnre => 1, hmhd => 1, iviv => 1, 'key ' => 1,
mdat => 1,   mdhd => 1, mp4a => 1, mp4s => 1, mpv4 => 1,   mvhd => 1, name => 1,
priv => 1,   stco => 1, stsc => 1, stsd => 1, stts => 1,   tkhd => 1, tref => 1,
user => 1,   vmhd => 1, apid => 1, geid => 1, wide => 1,
);

sub int64toN {
    my($int64) = @_;
    my $high32bits = pack('N', $int64 << 32);
    my $low32bits =  pack('N', $int64 & 0xffff);
    return $high32bits . $low32bits;
}

sub int64fromN{
    my($buf) = @_;
    my($high32bits, $low32bits) = unpack("NN", $buf);
    return ($high32bits*(2**32)) + $low32bits;
}

# begin class methods

sub new {
    my($class, %args) = @_;
    my $self = \%args;
    bless($self, $class);
    $self->{node} = Tree::Simple->new($self);
    $self->{parent} ||= 0;
    $self->{parent}->addChild($self->{node}) if ref $self->{parent};
    $self->Read($self->{Read}) if defined $self->{Read} and $self->{rbuf};
    return $self;
}

sub parent { return shift->{parent} } 

sub node   { return shift->{node} }

sub rbuf   { return shift->{rbuf} }

sub Read {
    my($self, $starting) = @_;
    $self->{start} = $starting;
    $self->{offset} = 8;
    ($self->{size}, $self->{type}) = 
      unpack 'Na4', substr(${$self->{rbuf}}, $starting, 8);
    if ($self->{size} == 1) {
        $self->{size} = int64fromN( substr($$self->{rbuf}, $starting + 8, 8) );
        $self->{offset} = 16;
    }
    return $self->{size};
}

sub type {
    my($self, $newtype) = @_;
    if(defined $newtype) {
        $self->{type} = substr($newtype, 0, 4);
        substr( ${$self->{rbuf}}, $self->{start} + 4, 4, $self->{type} );
    }
    return $self->{type};
}

sub start {
    my($self, $newstart) = @_;
    $self->{start} = $newstart if defined $newstart;
    return $self->{start};
}

sub size {
    my($self, $newsize) = @_;
    if(defined $newsize) {
        croak "Bad size $newsize in resize of atom" if $newsize < 8;
        croak "No support for atoms > 2**32 in size" 
          if $newsize > 2**32 or $self->{size} > 2**32;
        $self->{size} = $newsize;
        substr( ${$self->{rbuf}}, $self->{start}, 4, pack('N', $newsize) );            
    }
    return $self->{size};
}

sub offset {
    my($self, $o) = @_;
    $self->{offset} = $o if defined($o) and ($o == 8 or $o == 16);
    return $self->{offset};
}

sub data {
    my($self, $newdata) = @_;
    if(defined $newdata) {
        my $newsize = length $newdata + 8;
        my $diff = $newsize - $self->{size};
        $self->{size} = $newsize;
        substr($$self->{rbuf}, $self->{start} + $self->{offset}, 
          $self->{size} - $self->{offset}, $newdata);
        $self->redoStarts($diff);
    }
    return substr(${$self->{rbuf}}, $self->{start} + $self->{offset}, 
      $self->{size} - $self->{offset} );
}

sub root {
    my($self) = @_;
    return $self->node if $self->node->isRoot();
    return unless ref $self->{parent};
    return $self->{parent}->getNodeValue()->root();
}

sub getAllRelatives {
    my($self) = @_;
    my $visitor = Tree::Simple::Visitor->new();
    $self->root->accept($visitor);
    my @a = $visitor->getResults;
    return \@a;
}

sub AtomTree {
    my($self) = @_;
    my $view = Tree::Simple::View::HTML->new(
      $self->{node}, (
        list_css       => "list-style: circle;",
        list_item_css  => "font-family: courier;",
        node_formatter => sub { my ($tree) = @_;
          return "<em> " . $tree->getNodeValue->print() . " </em>"; },
      ) );
    return $view->expandAll();
}

sub resizeContainers {
    my($self, $diff) = @_;
    my $parent = $self->{parent};
    if($parent and ref $parent) {
        my $container = $parent->getNodeValue();
        if($container->type ne 'file') {
            $container->size($container->size + $diff);
            $container->resizeContainers($diff);
        }
    }
}

sub redoStarts {
    my($self, $diff) = @_;
    foreach my $atom ( @{ $self->getAllRelatives() } ) { 
        $atom->{start} += $diff 
          if $atom->start >= $self->start + $self->size + $diff;
    }
}

sub selfDelete {
    my($self) = @_;
    $self->resizeContainers( -$self->size);
    substr( ${$self->{rbuf}}, $self->start, $self->size, '' );
    $self->redoStarts( -$self->size);
    my $parent = $self->{parent};
    return unless ref $parent;
    $parent->removeChild($self->node);
    return 1;
}

sub insertNew {
    my($self, $type, $data, $before) = @_;
    my $node = $self->{node};
    my $atom = new Audio::M4P::Atom(parent => $node, rbuf => $self->{rbuf});
    my $after_atom;
    if( $before and ($after_atom = $self->Contained($before)) ) {
        $atom->{start} = $after_atom->{start};
    }
    else { $atom->{start} = $self->{start} + $self->{size} }
    $atom->{offset} = 8;
    $atom->{size} = 8 + length $data;
    $atom->{type} = $type;
    my $buf = pack('Na4', $atom->{size}, $type) . $data;
    substr(${$self->{rbuf}}, $atom->{start}, 0, $buf);
    $self->redoStarts($atom->{size});
    $self->size($self->{size} + $atom->{size});
    $self->resizeContainers($atom->{size});
    return $atom;
}

sub insertNewMetaData {
    my($self, $type, $data, $before) = @_;
    my $wrapper = $self->insertNew($type, '', $before);
    my $flag = ($type =~ /gnre|disk|trkn/) ? 0 : 
      ($type =~ /rtng/) ? 21 : 1;
    $wrapper->insertNew('data', pack ('NN', $flag, 0) . $data);
}

sub Container {
    my($self, $container_type) = @_;
    my $parent = $self->{parent};
    return unless $parent and ref $parent;
    my $parent_atom = $parent->getNodeValue();
    return $parent_atom if $parent_atom->{type} =~ /$container_type/i;
    return $parent_atom->Container($container_type);
}

sub Contained {
    my($self, $type) = @_;
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
    my($self) = @_;
    return $container_atom_types{$self->{type}};
}

sub print {
    my($self) = @_;
    return "Atom " . $self->type . " at " . $self->start . 
      " size ". $self->size . " ends at " . ($self->start + $self->size);
}


=head1 NAME

Audio::M4P::Atom -- M4P/MP4/M4A QuickTime audio music format atoms

=head1 DESCRIPTION
    
M4P is a QuickTime protected audio file format. It is composed of a linear
stream of bytes which are segmented into units called atoms. Some atoms
may contain other atoms. This module has methods for handling atoms which 
are delegated by the QuickTime and other modules in the Audio::M4p hierarchy.
   
=head1 AUTHOR 

    William Herrera B<wherrera@skylightview.com>. 

=head1 SUPPORT 

Questions, feature requests and bug reports should go to 
<wherrera@skylightview.com>.


=cut

1;

