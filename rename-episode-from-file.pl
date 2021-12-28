#!/usr/bin/perl -w -CSD
use strict;
use warnings FATAL => 'all';
use utf8;
use Encode;
#use open qw(:std :encoding(utf-8));
use Time::Local;
use Data::Dumper;
use Getopt::Long;
use Pod::Usage;
use HTML::TreeBuilder;

BEGIN {
    $0 =~ m=^((.*)[:/\\])?(\w+)(\..*)?$=;
    my ($ScriptDir) = $2 || "";
    unshift @INC, $ScriptDir;
}

my ($ScriptName) = $0;
$ScriptName =~ s=^(.*[:/\\])?(\w+)(\..*)?$=$2=;
my $startTime = time ();
my $isVerbose = 0;
my $isDryRun = 0;
my $isSummary = 1;
my $isMissing = 0;
my $isDuplicate = 0;
my $isKeepLibrary = 0;
my $isDumpLibrary = 0;
my $filterLibrary = undef;
my $isAllFiles = 0;
my $videoFileFilter = qr/\.mp(?:eg)?4$/i;
my $downloadTimeout = 2*60;

my %library;
my $newLibraryFound = 0;
$Data::Dumper::Sortkeys = 1;

sub basename #($file)
{
    my ($file) = @_;
    $file or return;

    my ($fn, $ext) = ($file =~ m{.*?([^\\/]+?)(?:\.([^\.]*))?$});
    wantarray and return ($fn, $ext);
    return join(".", ($fn, $ext));
}

sub renameFileSet #($oldFileName, $newFileName, $path);
{
    my ($oldFileName, $newFileName, $path) = @_;

    my ($oldFn, $oldExt) = basename($oldFileName);
    my ($newFn, undef) = basename($newFileName);

    my $globKey = "${path}/${oldFn}" . ($isAllFiles ? ".${oldExt}" : '.');
    my $rc = 0;
    foreach (<$globKey*>) {
        s/\\ / /g;
        s/\\'/'/g;

        if (-f $_) {
            my $source = $_;
            my (undef, $ext) = basename($source);
            my $target = "${path}/${newFn}.${ext}";
            $target =~ s/\\ / /g;
            $target =~ s/\\'/'/g;

            my $mtime = (stat($source))[9] or die "+++ cannot stat '${source}': $!\n";
            if (time() - $downloadTimeout < $mtime) {
                print STDERR "\nINFO     cannot rename: '${source}': file was changed few seconds ago, assumed running download (retry in ${downloadTimeout} sec. please)'\n";
                $rc = 99;
            }

            if ($isDryRun > 0) {
                print STDERR "\nINFO     renamed (--dry-run): '${source}' -> '${target}'\n";
                $rc = 1;
            } else {
                $rc = rename($source, $target) || die ( "+++ cannot rename '${source}' -> '${target}': $!\n");
                print STDERR "\nINFO     renamed: '${source}' -> '${target}'\n";
            }
        }
    }
    return $rc;
}

sub getRegEx #($rowHash);
{
    my %data = @_;
    my @regex;

    foreach my $key (sort keys %data) {
        $key =~ m/^-/ and next; # ignore meta keys

        (my $val = $data{$key}) =~ s#^\s*(.*)\s*$#$1#; # trim
        my $len = length($val);
        $len > 3 or next; # only content with more than 3 chars
        my $withDate = $len > 4 ? '(?:[\(_]?(?:\d{2}|\d{4})[\)_]?)?' : '';

        $val =~ s#[\.]#~~dot~~#g; # keep dot but make it optional
        $val =~ s#([_\s]+)#~~space~~#ug; # normalize space

        $val =~ s#(ß|ss)#(ß|ss)#iug; # handle special char "ß"
        $val =~ s#[^a-z0-9\.\?,:\-\(\)\|~]#.?#iug; # non greedy wildcard for any non ASCII char

        $val =~ s#(\d+)#0*$1#g; # allow leading zeros for numbers

        $val =~ s#~~space~~#[_ ]+?#g;
        $val =~ s#~~dot~~#\.?#g;

        $val =~ s#(\\[\.\?,:\-\(\)])#_?$1_?#g; # allow spaces before and after: dot, ...
        push @regex, "(-\\1?[-_]*?${val}_*?\\1?${withDate}[-_]*?\\1?-)"; # allow optional date (year)
    }
    return join ("|", @regex); # join all cell regex groups with "or"
}

sub getEpisodeKey #($season, $episode)
{
    my ($season, $episode) = @_;
    $season //= 1;
    $episode //= 0;
    return sprintf('S%02s_E%02s', $season, $episode);
}

sub readLibrary #($file)
{
    my @libraryFiles = @_;

    my $newEntries = 0;
    foreach my $library (@libraryFiles) {
        exists $library{'-libraryFiles'} and exists $library{'-libraryFiles'}{$library} and next;

        $isVerbose > 2 and print STDERR "TRACE     read library: '$library'\n";
        my $t = HTML::TreeBuilder->new;
        $t->utf8_mode(1);
        $t->ignore_unknown(0);
        $t->parse_file($library);
        my $entries = 0;

        my @tables = $t->find('table');
        my $tc = 0;
        foreach my $table (@tables) {
            my %db;
            my @fields;
            my $libKey = "${library}::" . $tc++;

            # search season (before table)
            my $season = 1;
            my @before = $table->right();
            if (@before) {
                my $seasonPattern = qr/(season|staffel)_?/i;
                my $season_element = $before[0]->look_down('id' => qr/^(season|staffel).*$/i);
                if (ref $season_element eq "HASH") {
                    ($season = $season_element->id) =~ s#\s*$seasonPattern\s*##i;
                }
            }

            my @head = $table->find('th');
            foreach my $head (@head) {
                push(@fields, $head->as_text_trimmed);
            }

            my @rows = $table->find('tr');
            my $r = 0;
            foreach my $row (@rows) {
                my %data;

                # remove useless data
                map {$_->delete} $row->find('small');
                map {$_->delete} $row->find('sup');

                my $key = $row->as_text_trimmed;

                my @cells = $row->find('td');
                my $i = 0;
                my $id;
                foreach my $cell (@cells) {
                    if ($cell->id) {
                        $id = $cell->id;
                    }
                    if (@fields) {
                        $data{$fields[$i++]} = $cell->as_text_trimmed;
                    } else {
                        $data{$i++} = $cell->as_text_trimmed;
                    }
                }

                if (keys %data) {
                    grep /titel|title/i, keys %data or next;

                    $data{'-content'} = $key;
                    $data{'-library'} = $libKey;
                    $data{'-regex'} = getRegEx(%data);
                    $data{'-season'} = $season;
                    $data{'-episode'} = exists $data{'Nr. (St.)'} ? $data{'Nr. (St.)'} :
                        exists $data{'Folge'} ? $data{'Folge'} : $data{'Nr.'};
                    $data{'-se_key'} = getEpisodeKey ($data{'-season'}, $data{'-episode'});
                    $data{'-se_key'} =~ s#[^a-z0-9_]#_#ig;
                    $id or $id = $data{'-se_key'};
                    (exists $db{$id} or !$id) and $id = $r++;
                    $data{'-id'} = $id;
                    $newEntries++;
                    $db{$id} = \%data;
                }
            }

            if (keys %db) {
                $newLibraryFound++;
                $library{$libKey} = \%db;
                my @keys = sort keys %db;
                $newEntries and $db{'-rowKeys'} = \@keys;
                $entries += scalar @keys;
            }
        }

        $isVerbose > 2 and print STDERR "\nTRACE  library '$library' with: ${entries} entries\n";
        $isVerbose > 2 and print STDERR "TRACE  ---------------------------------------------------------------------------------------------\n";
    }

    my @keys = sort keys %library;
    my $libraryTables = scalar @keys or die "FATAL  +++ no library entries found!!!\n";
    if ($newEntries > 0) {
        $library{'-libraryKeys'} = \@keys;
        my %fileHash = map {($_, 1)} @libraryFiles;
        $library{'-libraryFiles'} = \%fileHash;
        $isDumpLibrary and print STDERR Dumper(\%library);
    }
    $isVerbose > 2 and print STDERR "\nTRACE  library tables found: ${libraryTables} (in " . (scalar @libraryFiles) . " HTML files)\n";
    $isVerbose > 2 and print STDERR "TRACE  ---------------------------------------------------------------------------------------------\n";
}

sub checkFile #($fileName, $path)
{
    my ($fileName, $path) = @_;
    $isVerbose > 2 and print STDERR "TRACE    checking file: '${fileName}'\n";

    $isAllFiles and $fileName =~ m/\.html$/ and return; # skip library files

    my $bn = basename($fileName);
    my $bn_enc = !Encode::is_utf8($bn) ? Encode::decode('utf-8', $bn) : $bn;
    my $rc = 0;
    foreach my $libraryFile (@{$library{'-libraryKeys'}}) {
        $libraryFile =~ m/^-/ and next;
        my $db = $library{$libraryFile};

        foreach my $rowKey (@{$db->{'-rowKeys'}}) {
            $rowKey =~ m/^-/ and next;
            my $row = $db->{$rowKey};

            # match: (S01_E02_)?(Thema)(<search pattern from HTML [regex generator: getRegEx]>)
            my $pattern = "^(?:(?:S[^_]+_E[^_]+|$row->{'-se_key'})_)?([^-]+)(-[^-]+)?($row->{'-regex'})";
            my $re = qr/$pattern/ui;
            if ($bn_enc =~ $re) {
                my $match = $1;

                $isVerbose > 2 and print STDERR "TRACE    * matched '$bn' with '$pattern' from '${rowKey}'\@'${libraryFile}'\n";

                if ($bn =~ qr/^(S[^_]+_E[^_]+|$row->{'-se_key'})_/) {
                    $isVerbose and print STDERR "WARN     matched '$bn' already contains series key '$row->{'-se_key'}', keeping current file name.\n";
                } else {
                    my $newFileName = "$row->{'-se_key'}_${bn}";
                    my $renRc = renameFileSet($bn, $newFileName, $path);
                    if ($renRc == 99) {
                        $row->{'-downloading'} = 1;
                    } else {
                        # use new name for stats
                        $bn = $newFileName;
                    }
                }

                if ($bn =~ $videoFileFilter) { # don't collect all files if $isAllFiles was set
                    $row->{'-matched'}++;
                    if (exists $row->{'-matchedFile'}) {
                        push @{$row->{'-matchedFile'}}, ( $bn . ($isVerbose ? "  [match::${match}]" : '') );
                    }
                    else {
                        $row->{'-matchedFile'} = [ $bn . ($isVerbose ? "  [match::${match}]" : '') ];
                    }
                }

                $rc++;
                last;
            }
        }

        $rc and last;
    }
    $rc or do {$isVerbose and print STDERR "WARN     +++ no match for '$bn'\n"};
    $isVerbose > 2 and print STDERR "TRACE  ---------------------------------------------------------------------------------------------\n";
}

my %seenDir;
my %multiDir;
sub walkDir #($item)
{
    my ($item) = @_;

    (my $globDir = $item) =~ s{\\}{/}g;
    $globDir =~ s/ /\\ /g;
    $globDir =~ s/'/\\'/g;
    foreach (<$globDir/*>) {
        s/\\ / /g;
        s/\\'/'/g;
        if (-d $_) {
            (my $subDir = $_) =~ s/^$globDir//;
            if (exists $seenDir {$subDir}) {
                $isVerbose > 2 and print STDERR "\nTRACE  already seen folder: ".$seenDir {$subDir};
                exists $multiDir {$subDir} or $multiDir {$subDir} = $seenDir {$subDir};
                $multiDir {$subDir} .= "\n\t$_";
            }
            $seenDir {$subDir} = $_;
            $isVerbose > 2 and print STDERR "\nTRACE  scanning folder: '$_'\n";
            $isVerbose > 2 and print STDERR "TRACE  ---------------------------------------------------------------------------------------------\n";
            walkDir ($_);
        } elsif (-f $_ && ($isAllFiles || $_ =~ $videoFileFilter)) {
            checkFile ($_, $globDir);
        }
    }

    return 1;
}

sub printMissingSeries #()
{
    my $dup = 0;
    my $mis = 0;
    my $seen = 0;
    my $skip = 0;

    my %filter = ('missing' => 0, 'seen' => 0, 'duplicate' => 0);

    foreach my $libraryFile (@{$library{'-libraryKeys'}}) {
        $libraryFile =~ m/^-/ and next;
        my $db = $library{$libraryFile};

        foreach my $rowKey (@{$db->{'-rowKeys'}}) {
            $rowKey =~ m/^-/ and next;
            my $row = $db->{$rowKey};

            my $info = "";
            foreach my $key (sort keys %$row) {
                $key =~ m/^-/ and next;
                $info .= sprintf("         %-40s: %s\n", $key, $row->{$key});
            }
            if (exists $row->{'-matched'}) {
                $row->{'-downloading'} and $skip++;
                if ($row->{'-matched'} > 1) {
                    $dup++;
                    if (defined $filterLibrary) {
                        $row->{'-content'} =~ $filterLibrary and ++$filter{'duplicate'} or next;
                    }
                    $isDuplicate or next;
                    print STDERR "ERROR  duplicate found:\n${info}";
                    print STDERR "         Files:\n";
                    print STDERR "         - ", join("\n         - ", @{$row->{'-matchedFile'}}), "\n";
                } else {
                    $seen++;
                    if (defined $filterLibrary) {
                        $row->{'-content'} =~ $filterLibrary and ++$filter{'seen'};
                    }
                    next;
                }
            } else {
                $mis++;
                if (defined $filterLibrary) {
                    $row->{'-content'} =~ $filterLibrary and ++$filter{'missing'} or next;
                }
                $isMissing or next;
                print STDERR "ERROR  missing episode:\n${info}";
            }
            $isVerbose > 2 and print STDERR "TRACE  ---------------------------------------------------------------------------------------------\n";
        }
    }

    print STDERR "INFO   ---------------------------------------------------------------------------------------------\n";
    print STDERR "INFO   Summary:\n";
    print STDERR "INFO   - episodes found:      " . sprintf("%10s", $seen, ) . ($skip ? sprintf(" / skipped: %10s (try again in about $downloadTimeout sec.)", $skip) : '') . "\n";
    print STDERR "INFO   - episodes missing:    " . sprintf("%10s", $mis) . "\n";
    print STDERR "INFO   - duplicate matches:   " . sprintf("%10s", $dup) . "\n";
    if (defined $filterLibrary) {
        print STDERR "INFO   ---------------------------------------------------------------------------------------------\n";
        print STDERR "INFO   - episodes found by '$filterLibrary':      " . sprintf("%10s", $filter{'seen'}) . "\n";
        print STDERR "INFO   - episodes missing by '$filterLibrary':    " . sprintf("%10s", $filter{'missing'}) . "\n";
        print STDERR "INFO   - duplicate matches by '$filterLibrary':   " . sprintf("%10s", $filter{'duplicate'}) . "\n";
    }
}

sub main #()
{
    my @rootDirs = ();
    my $filterPattern;
    my $isHelp = 0;
    my $isUsage = 0;

    # auto flush
    binmode STDIN,  ":unix";
    binmode STDOUT, ":unix";
    binmode STDERR, ":unix";

    Getopt::Long::Configure ("no_ignore_case", "bundling_override");
    GetOptions (
        "folder|f=s@"        => \@rootDirs,
        "summary|s!"         => \$isSummary,
        "show-missing|M!"    => \$isMissing,
        "show-duplicates|D!" => \$isDuplicate,
        "dry-run|dr!"        => \$isDryRun,
        "keep-library|kl!"   => \$isKeepLibrary,
        "dump-library|dl!"   => \$isDumpLibrary,
        "verbose|v+"         => \$isVerbose,
        "help|h"             => \$isHelp,
        "usage|u"            => \$isUsage,
        "episode-filter|e=s" => \$filterPattern,
        "scan-all!"          => \$isAllFiles,
    ) or pod2usage(-exitval => 9);
    $isUsage and pod2usage(-exitval => 8);
    $isHelp  and pod2usage(-exitval => 8, -verbose => 2);

    $filterPattern and $filterLibrary = qr/$filterPattern/i; #ex.: (M.*?nster|Thiel|Boerne)
    @rootDirs or pod2usage(
        -message => "+++ no folder provided!",
        -exitval => 9
    );

    $isVerbose > 2 and print STDERR "TRACE  ---------------------------------------------------------------------------------------------\n";
    $isVerbose > 2 and print STDERR "TRACE  Script:           $0\n";
    $isVerbose > 2 and print STDERR "TRACE  OS:               $^O\n";
    $isVerbose > 2 and print STDERR "TRACE  Perl:             $^X\n";
    $isVerbose > 2 and print STDERR "TRACE  Includes:         ".join ("\n                  ", (grep {!m/^.$/} map { $^O =~ m/MSwin/i and s{/}{\\}g; $_; } @INC))."\n";
    $isVerbose > 2 and print STDERR "TRACE  Perl Version:     $^V ($])\n";
    $isVerbose > 2 and print STDERR "TRACE  PID:              $$\n";
    $isVerbose > 2 and print STDERR "TRACE  Root folders:     ".join ("\n                  ", @rootDirs)."\n";

    foreach my $rootDir (@rootDirs) {
        $rootDir =~ s#[/\\]*$##g;
        $newLibraryFound = 0;
        $isKeepLibrary or %library = ();
        print STDERR "INFO   ---------------------------------------------------------------------------------------------\n";
        print STDERR "INFO   scanning folder: '${rootDir}'\n";
        print STDERR "INFO   ---------------------------------------------------------------------------------------------\n";
        readLibrary (glob ("'$rootDir'/*.html"));
        walkDir ($rootDir);
        $isKeepLibrary and $newLibraryFound and print STDERR "INFO   keeping library from folder: '${rootDir}'\n";
    }
    $isVerbose > 2 and print STDERR "TRACE  ---------------------------------------------------------------------------------------------\n";
    $isSummary and printMissingSeries();
    print STDERR "INFO   duration ".(time () - $startTime)." sec.\n";
    return 1;
}

main ();

__END__

=head1 NAME

B<rename-episode-from-file.pl>  Copyright (C) 2021 Awalon
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it under certain conditions.

B<rename-episode-from-file.pl> - rename movie files based on season list defined by wikipedia list (HTML).

=head1 SYNOPSIS

B<rename-episode-from-file.pl> [options]

 Options:

    --folder=<folder>*,-f <folder>
                            list of folders which will be processed
    --summary,-s            show summary (disable with --no-summary)
    --show-missing,-M       show missing episodes
    --show-duplicates,-D    show duplicate files
    --dry-run,-dr           simulation only, don't rename files
    --keep-library,-kl      keep library for next folder in list, instead of library reset per folder
    --dump-library,-dl      dump parsed HTML file
    --verbose,-v            be verbose
    --help,-h               show help
    --usage,-u              show this usage
    --episode-filter=<regex>,-e <regex>
                            filter episodes provided by HTML file by regular expression
    --scan-all              experimental: Scan and rename all file types

 Ex.:
    rename-episode-from-file.pl --dry-run --show-duplicates --keep-library --folder "/home/plex/series/Tatort" --folder "/home/plex/series-disk2/Tatort"

=head1 OPTIONS

=over 4

=item B<--folder>, B<-f>

Folder with HTML (*.html) and mp4 (*.mp4) files which will be recursively processed.

For each folder all HTML files will be parsed for tables having episode data like:
  L<"Tatort" TV Series - Episodes|https://de.wikipedia.org/wiki/Liste_der_Tatort-Folgen>

This file will be matched against .mp4 files to add B<"SE<lt>seasonE<gt>_EE<lt>episodeE<gt>_"> prefix
for all files having same name including different file extensions (like subtitle files).

=item B<--summary>

Show summary

=item B<--show-missing>, B<-M>

Show episodes from HTML files without a corresponding mp4 file.

=back

=head1 DESCRIPTION

B<rename-episode-from-file.pl> will read HTML files having episode data and
rename movie file set (incl. subtitle etc.) with an season and episode prefix.

Intention:
- Fix order of files by season and episode
- Detect duplicates and missing episodes
- Automatic detection by Plex Media Server

=cut