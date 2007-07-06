use strict;  
# kwalitee, schmalitee...
# buggy CPANTS wants even dumb doc placeholder modules to use strict...

=head1 NAME

Audio::M4P -- Perl QuickTime / MP4 / iTunes Music Store audio / video file tools

=head1 DESCRIPTION
    
M4P is a QuickTime protected audio file format.

=head1 SYNOPSIS

    use Audio::M4P::QuickTime;

    my $mp4file = "file.m4p";
    my $qt = new Audio::M4P::QuickTime(file => $mp4file);
    my $tags = $qt->GetMetaInfo;
    print "Artist is $tags->{ARTIST}\n" if $tags->{ARTIST};


    use Audio::M4P::Decrypt;

    my $outfile = 'mydecodedfile';
    my $deDRMS = new Audio::M4P::Decrypt;
    $deDRMS->DeDRMS($mp4file, $outfile);

    See also the individual pod documentation for Audio::M4P::QuickTime
    and Audio::M4P::Decrypt.

    
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

=cut

1;
