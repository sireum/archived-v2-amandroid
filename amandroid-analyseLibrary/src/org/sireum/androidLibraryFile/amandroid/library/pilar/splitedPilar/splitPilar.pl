#!/usr/bin/env perl

use strict;
use warnings;

#my $dh = $ARGV[0];

#closedir $dh;

my $dir = glob('..');

opendir(DIR, $dir) or die $!;

while (my $file = readdir(DIR)) {

	# We only want files with .pilar suffix
	next unless (-f "$dir/$file");
	next unless ($file =~ m/\.pilar$/);
	print "$file\n";


open(my $infh, '<', "$dir/$file") or die $!;

my $outfh;
my $filecount = 0;

my $recsInFile = 0;

close($outfh) if $outfh;
open($outfh, '>', sprintf('%sOut%03d.plr', $file, ++$filecount)) or die $!;

while ( my $line = <$infh> ) {

	    if ( $line =~ /^record \[\|/ ) {  # if line is starting with "record [|"

             ++$recsInFile;
             if( $recsInFile > 99 ) {
			   close($outfh) if $outfh;
			   $recsInFile = 0;
			   open($outfh, '>', sprintf('%sOut%03d.plr', $file, ++$filecount)) or die $!; #sprintf('$file%03d.pilar', ++$filecount)) or die $!;
			 }
	    }

		print {$outfh} $line or die "Failed to write to file: $!";
}

close($outfh);
close($infh);

}
