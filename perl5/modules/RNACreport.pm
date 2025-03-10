use strict;
use warnings;
use File::Temp qw/ tempfile /;
use Cwd;
require 'align-nmers.pm';
require Exporter;

our @ISA    = qw(Exporter);
our @EXPORT = qw(generate_html_report);

sub generate_html_report{
	my ($inDir,$sBatchID,$nMotifSize,$suffix,$setAFile,$setBFile) = @_;
	print "generating html report for $suffix normalization...\n";
	$setAFile = "$inDir/${nMotifSize}mer_trimmedmeans_setA_$suffix.txt" if (!$setAFile || $setAFile eq '');
	$setBFile = "$inDir/${nMotifSize}mer_trimmedmeans_setB_$suffix.txt" if (!$setBFile || $setBFile eq '');
	my $outFileName = "RNAcompete_report_$suffix.html";
	my $outDir = $inDir . '/report_' . $suffix ;
	my $outFile = $outDir .'/'. $outFileName;
	my $logoDir =  "logo_images";
	my $scatterDir = "scatter_plots";
	# make directories;
	mkdir($outDir) unless -e $outDir;
	mkdir($outDir.'/'.$logoDir) unless -e $logoDir;
	mkdir($outDir.'/'.$scatterDir) unless -e $scatterDir;
	
	my $rasHeaders = _get_headers($setAFile);

	#print join("\n",@{$rasHeaders})."\n";
	
	#print initial info
	open(my $out, ">$outFile") or die "couldn't open $outFile\n";
	
	my $title = "RNAcompete results for batch \"$sBatchID\"";
	my $nmer = "${nMotifSize}mer";
	
	print $out "<html>\n<head>\n<title>$title</title>\n</head>\n";
	print $out "<body>\n<h1>$title</h1>\n";
	print $out "<h2>Results based on $nmer analysis with $suffix normalization -- ";
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime;
	$year += 1900;
	$mon += 1;
	my $datetime = sprintf "%02d-%02d-%04d %02d:%02d", $mday, $mon, $year, $hour, $min;
	print $out "generated on $datetime</h2>\n";
	
	
	my $nSamples = (scalar @{$rasHeaders})-1;
	for (my $nCol=1; $nCol<=$nSamples ; $nCol++){
		my $sSample = $rasHeaders->[$nCol];	
		$sSample =~ s/\s//g;
		print "$sSample\n";
		print $out "<p><a href=\"#$sSample\">Results for $sSample</a></p>\n";
	}
	
	
	
	
	# create all the scatterplots + correlations
	_get_scatter($outDir.'/'.$scatterDir,$setAFile,$setBFile);
	
	
	my %sampleToABPearson = ();
	my %sampleToABSpearman = ();
	# read in scatterplot correlations
	my $corFile ="$outDir/$scatterDir/setA_setB_correlations.txt"; 
	open(my $corfh, $corFile) or die "couldn't open $corFile\n";
	while(<$corfh>){
		chomp; chop while /\r/;
		my ($s,$pr,$sr) = split("\t");
		$sampleToABPearson{$s} = $pr;
		$sampleToABSpearman{$s} = $sr;
	}
	close($corFile);

	#print results for each protein
	#print top 10 Nmers;
	for (my $nCol=1; $nCol<=$nSamples ; $nCol++){
		my $sSample = $rasHeaders->[$nCol];	
		$sSample =~ s/\s//g;
		print $out "<hr style=\"width: 70%\"/>\n";
		print $out "<a name=\"$sSample\"></a>\n";
		print $out "<h2>Results for $sSample</h2>\n";
		#get scatterplots and print
		my $scatterFile = "$scatterDir/${sSample}_setAsetBscatter.png";
		print $out "<h3>Set A vs Set B $nmer scatterplot</h3>\n";
		my $pr = $sampleToABPearson{$sSample};
		my $sr = $sampleToABSpearman{$sSample};
		print $out "<p>Pearson R=${pr} Spearman R=${sr}</p>\n";
		print $out "<img src=\"$scatterFile\" />\n";;
		
		# get top 10, top 20, top 100 logos
		my @anTop = (10,20,100);
		my $raraAllTopNmersSetA = _get_top_nmers($setAFile,$nCol,\@anTop);
		my $raraAllTopNmersSetB = _get_top_nmers($setBFile,$nCol,\@anTop);
		foreach (my $iTop=0; $iTop <= $#anTop; $iTop++){
			my $nTop = $anTop[$iTop];
			my $rasTopnmersSetA = $raraAllTopNmersSetA->[$iTop];
			my $rasAlignedTopSetA = align_and_print($rasTopnmersSetA);
			my $rasTopnmersSetB = $raraAllTopNmersSetB->[$iTop];
			my $rasAlignedTopSetB = align_and_print($rasTopnmersSetB);
		
			my $tableHeader =  "<table border=1 cellpadding=10px>\n<tr><th>Set A</th><th>Set B</th></tr>\n";
	
			# print the nmers for the top 10 only
			if($nTop == 10){
				_print_top_nmers($out,$tableHeader,$nTop,$rasTopnmersSetA,$rasAlignedTopSetA,$rasTopnmersSetB,$rasAlignedTopSetB);
				my $alignedoutfile = "$outDir/${sSample}_top${nTop}_aligned_nmers.txt";
				_print_top_nmers_tofile($alignedoutfile,$nTop,$rasAlignedTopSetA);
			}

			my $logoFileSetA = "$logoDir/${sSample}_logo_SetA_top$nTop";
			my $logoFileSetB = "$logoDir/${sSample}_logo_SetB_top$nTop";
		
			my $return = _get_logo("$outDir/$logoFileSetA",$rasAlignedTopSetA);
			$return = _get_logo("$outDir/$logoFileSetB",$rasAlignedTopSetB);
			
			print $out "<h3>Logo generated from top $nTop ${nmer}s</h3>\n";
			print $out $tableHeader;
			print $out "<tr>\n";
			print $out "<td><img src=\"${logoFileSetA}.png\" /></td>\n";
			print $out "<td><img src=\"${logoFileSetB}.png\" /></td>\n";
			print $out "</tr></table>\n";
		}
		
	}
	
	
	
	print $out "</body>\n</html>\n";
	
	close($out);
}

sub _get_scatter{
	my ($scatterDir,$setAFile,$setBFile) = @_;
	my $matlab = join('; ',
						"outdir = '$scatterDir'",
						"addpath('/Users/eskay/Documents/work/grad/Projects/RNAcompeteDB/working/rnacompete/trunk/matlab')",
						"setAFile = '$setAFile'",
						"setBFile = '$setBFile'",
						"run_makescatters",
						"exit;");
	`matlab -nodesktop -nosplash -nodisplay -r "$matlab"  1>&2`;
}

sub _print_top_nmers{
	my ($out,$tableHeader,$nTop,$rasTopnmersSetA,$rasAlignedTopSetA,$rasTopnmersSetB,$rasAlignedTopSetB) = @_;
	# print top 10 nmers
	print $out "<h3>Top $nTop Nmers</h3>\n";
	print $out $tableHeader;
	print $out "<tr>\n";
	print $out "<td>\n<code>".join("<br/>\n",@{$rasTopnmersSetA})."</code>\n</td>\n";
	print $out "<td>\n<code>".join("<br/>\n",@{$rasTopnmersSetB})."</code>\n</td>\n";
	print $out "</tr></table>\n";
	# print aligned top 10 nmers
	print $out "<h3>Top $nTop Nmers, aligned</h3>\n";
	print $out $tableHeader;
	print $out "<tr>\n";
	print $out "<td>\n<code>".join("<br/>\n",@{$rasAlignedTopSetA})."</code>\n</td>\n";
	print $out "<td>\n<code>".join("<br/>\n",@{$rasAlignedTopSetB})."</code>\n</td>\n";
	print $out "</tr></table>\n";

}

sub _print_top_nmers_tofile{
	my ($outFile,$top,$rasAlignedd) = @_;
	open(my $out, ">$outFile") or die "couldn't open $outFile\n";
	# print top 10 nmers
	print $out join("\n",@{$rasAlignedd})."\n";
	close($out);
}


sub _get_logo{
	my ($logoFile,$rasAligned) = @_;
	# write temporary .fa file
	my($fh,$tmpFaFile) = tempfile(UNLINK => 1);
	my $i = 1;
	my $alignWidth;
	foreach my $seq (@{$rasAligned}){
		print $fh ">seq_$i\n$seq\n";
		$alignWidth = length($seq) if $i == 1;
		$i++;
	}
	$logoFile =~ s/[\(\)]/_/g;
	my $cmd = "(weblogo -F png  -A rna --errorbars NO --color-scheme classic --fineprint '' --weight 0.0001 < $tmpFaFile > ${logoFile}.png) &> /dev/null";
#	my $cmd = "(weblogo -F png  -A rna --errorbars NO --color-scheme classic --fineprint '' --weight 25 < $tmpFaFile > ${logoFile}.png) &> /dev/null";
#	my $width = $alignWidth * 41 + 2.7152;
#	$width = int($width + .5 * ($width <=> 0)); # round it
#	my $dir = getcwd;
#	my $cmd = "~/Documents/work/grad/software/enologs_v1.40/enologos -f $tmpFaFile -c /Users/eskay/Documents/work/grad/software/enologos_v1.40/parameters.txt -o ${dir}/${logoFile}.ps}; convert -trim -density ${width}x200! -resize ${width}x200! ${dir}/${logoFile}.ps ${dir}/${logoFile}.png\n";
	#3print $cmd."\n";
	`$cmd`;
	close($fh);
	return $logoFile;
}

sub _get_top_nmers{
	my ($inFile,$nCol,$nTops) = @_;
	my $nSortCol = $nCol+1;
	my($fh,$tmpFile) = tempfile(UNLINK => 1);
	my $cmd = "sort -nrk $nSortCol $inFile > $tmpFile";
	#print "$cmd\n";
	`$cmd`;
	open(my $in, "$tmpFile") or die "couldn't open $tmpFile\n";
	#my $sLine = <$in>; #header
	my @araResult = ();
	for my $nTop (@{$nTops}){
		my @topResult = ();
		while(<$in>){
			my $sNmer = (split("\t"))[0];
			push(@topResult, $sNmer);
			last if $#topResult == ($nTop-1);
		}
		push(@araResult,\@topResult);
	}
	close($fh);
	return \@araResult;
}

sub _get_headers{
	my $inFile = shift;
	open(my $in, $inFile) or die "couldn't open $inFile\n";
	my $sLine = <$in>;
	chomp ($sLine); chop($sLine) while $sLine =~ /\r/;
	close($in);
	my @asHeaders = split("\t",$sLine);
	return \@asHeaders;
}
