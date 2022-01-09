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
my $useEpisodeOfYear = 0;
my $isKeepLibrary = 0;
my $isDumpLibrary = 0;
my $filterLibrary = undef;
my $isAllFiles = 0;
my $videoFileFilter = qr/\.mp(?:eg)?4$/i;
my $downloadTimeout = 2*60;

my %library;
my $newLibraryFound = 0;
$Data::Dumper::Sortkeys = 1;

my $cError = 0;
my $cFatal = 0;
my $cWarn  = 0;
sub logger #($tag, $message)
{
    my ($tag, @message) = @_;
    my ($sec, $min, $hour, $day, $mon, $year) = localtime();
    my $ts = sprintf("%04d-%02d-%02d %02d:%02d.%02d", $year+1900, $mon+1, $day, $hour, $min, $sec);

    if ($tag eq "E") {
        $cError++;
        $tag = "ERROR";
    } elsif ($tag eq "F") {
        $tag = "FATAL";
        $cFatal++;
    } elsif ($tag eq "W") {
        $tag = "WARN";
        $cWarn++;
    } elsif ($tag eq "T") {
        $isVerbose or return 1;
        $tag = "TRACE" ;
    }  else {
        $tag = "INFO";
    }
    print (join('', map {sprintf("%s %-6s %s\n", $ts, $tag, $_) } @message));
    $cFatal > 0 and die "";
    return 1;
}

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
    foreach (glob("${globKey}*")) {
        s/\\ / /g;
        s/\\'/'/g;

        if (-f $_) {
            my $source = $_;
            my (undef, $ext) = basename($source);
            my $target = "${path}/${newFn}.${ext}";
            $target =~ s/\\ / /g;
            $target =~ s/\\'/'/g;

            my $mtime = (stat($source))[9] or logger ('F', "+++ cannot stat '${source}': $!");
            if (time() - $downloadTimeout < $mtime) {
                logger ('I', "cannot rename: '${source}': file was changed few seconds ago, assumed running download (retry in ${downloadTimeout} sec. please)'");
                return 99;
            }

            if ($isDryRun > 0) {
                logger ('I', "renamed (--dry-run): '${source}' -> '${target}'");
                $rc = 1;
            } else {
                $rc = rename($source, $target) || logger ('F', "+++ cannot rename '${source}' -> '${target}': $!");
                logger ('I', "renamed: '${source}' -> '${target}'");
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
        my $withEpisode = $len > 4 ? '(?:[\(_]*?(?:episode|folge)_\d+[\)_]*?)?' : '';

        $val =~ s#[\.]#~~dot~~#g; # keep dot but make it optional
        $val =~ s#([_\s]+)#~~space~~#ug; # normalize space
        $val =~ s#[\\/]#~~slash~~#g; # mask '/' and '\' and make them optional

        $val =~ s#([\.\?,:\-\(\)])#_?\\$1_?#g; # mask and allow spaces before and after: dot, ...

        $val =~ s#(ß|ss)#(ß|ss)#iug; # handle special char "ß"
        $val =~ s#[^a-z0-9\\_\.\?,:\-\(\)\|~]#.*?#iug; # non greedy wildcard for any non ASCII char

        $val =~ s#(\d+)#0*$1#g; # allow leading zeros for numbers

        $val =~ s#~~slash~~#.?#g;
        $val =~ s#~~space~~#[_ ]*?#g;
        $val =~ s#~~dot~~#\\.?#g;

        push @regex, sprintf "(-\\2?[-_]*?%s%s_*?\\2?%s[-_]*?\\2?-)", $withEpisode, $val, $withDate; # allow optional date (year)
    }
    return join ("|", @regex); # join all cell regex groups with "or"
}

sub getEpisodeKey #($season, $episode, ?$pattern)
{
    my ($season, $episode, $pattern) = @_;
    $pattern //= 'S%03s_E%04s';
    $season //= 1;
    $episode //= 0;
    # remove leading season from episode
    $episode =~ s/^$season[\.,_]0*([^0]+\d*)$/$1/g;
    return sprintf($pattern, $season, $episode);
}

sub getNoInSeries #(@columns)
{
    my @columns = @_;

    # pattern will search for
    # (No.|Nr.|#)(up to short 3 chars words)(folge|episode|series|staffel|st.)(up to short 3 chars words)(staffel|season)(no. of season)
    my %options = map {
            $_, length($_)
        }
        grep {
            m/^\s*((N[ro]\.?)|#)?\s*(\w{1,3}\s+)?(\(?(folge|episode)\)?)?\s*(\w{1,3}\s+)?(\(?(series|st(affel|\.)?)(\s*\d+)?\)?)?\s*$/i
        } @columns;
    # Ex. by prio:
    # 'Nr. (St.)'
    # 'No. in series'
    # 'Folge'
    # 'Nr.'
    # ... sort by length
    my @prio = (sort { $options{$a} <=> $options{$b} } keys %options);
    return scalar @prio ? $prio[-1] : undef;
}

sub readLibrary #($file)
{
    my @libraryFiles = @_;

    my $newEntries = 0;
    foreach my $library (@libraryFiles) {
        exists $library{'-libraryFiles'} and exists $library{'-libraryFiles'}{$library} and next;

        $isVerbose > 1 and logger ('T', "read library: '$library'");
        my $t = HTML::TreeBuilder->new;
        $t->utf8_mode(1);
        $t->ignore_unknown(0);
        $t->parse_file($library);
        my $entries = 0;

        my @tables = $t->find('table');
        my $tc = 0;
        TABLE: foreach my $table (@tables) {
            my %db;
            my @fields;
            my $libKey = "${library}::" . $tc++;

            # search for nested table and skip parent tables
            my @nestedTables = $table->find('table');
            scalar @nestedTables > 1 and next TABLE;

            # search season (before table)
            my $season;
            my @before = $table->left();
            if (@before) {
                foreach my $elem (reverse @before) {
                    my @seasonMatches = $elem->look_down('id' => qr/^.*?(season|staffel).*$/i);
                    foreach my $seasonElement (reverse @seasonMatches)  {
                        if (ref $seasonElement eq 'HTML::Element') {
                            ($season = $seasonElement->id) =~ s#^.*?(\d+).*?$#$1#i;
                            $season and last;
                        }
                    }
                    $season and last;
                }
            }
            $season //= 1;

            my @head = $table->find('th');
            my $parentRow;
            my $maxRows = 1;
            my $curRow = 0;
            my $curCol = 0;
            my $colCount = 0;
            my %colSpans = ('-end' => undef);
            my %rowSpans = ();
            foreach my $head (@head) {
                my $currentValue = $head->as_text_trimmed;
                $isVerbose > 2 and logger ('T', "extract column field: $currentValue");

                my $parent = $head->parent;
                $parentRow //= $parent;
                if (!$parentRow->same_as($parent)) {
                    $parentRow = $parent; # next row
                    $curRow++;
                    $curCol = 0;
                    $curCol > $colCount and $colCount = $curCol;
                    $colSpans{'-end'} = undef;
                }

                if ($curRow > 0) {
                    exists $rowSpans{$curCol} and $rowSpans{$curCol}-- > 0 and $curCol++;
                }
                if (defined $head->attr('rowspan')) {
                    $head->attr('rowspan') > $maxRows and $maxRows = $head->attr('rowspan');
                    $rowSpans{$curCol} = $head->attr('rowspan');
                } else {
                    $rowSpans{$curCol} = 0;
                }
                if (defined $head->attr('colspan')) {
                    $colSpans{$curCol} = $head->attr('colspan');
                    if ($curRow == 0) {
                        push(@fields, ('') x $colSpans{$curCol}); # fill undef (colspan) with next row
                    }
                    $curCol += $colSpans{$curCol};
                    $curCol > $colCount and $colCount = $curCol;
                } else {
                    # search for th having title attribute defined
                    my @title = $head->look_down('title' => qr/^.+$/i);
                    my $columnName = scalar @title ? $title[-1]->attr('title') : $currentValue;
                    if ($curRow > 1 && (exists $colSpans{$curCol} || $colSpans{'-end'})) {
                        # fill values for colspan fields
                        $fields[$curCol] = $columnName;
                        $colSpans{'-end'} //= $colSpans{$curCol};
                        $colSpans{'-end'} -= 1;
                    } else {
                        if ($curRow == 0) {
                            push(@fields, $columnName);
                        } else {
                            $fields[$curCol] = $columnName
                        }
                    }
                    $curCol++;
                }
            }

            #my @rows = $table->find('tr');
            my @rows = $table->look_down(_tag => 'tr');
            my $r = 0;
            my $episodeColumn = getNoInSeries (@fields);
            %rowSpans = ();
            ROWS: foreach my $row (@rows) {
                my %data;

                # remove useless data
                map {$_->delete} $row->find('small');
                map {$_->delete} $row->find('sup');

                my $key = $row->as_text_trimmed;

                my @cells = $row->look_down(_tag => 'td');
                my $cellsCount = scalar @cells;
                $cellsCount > 0 or next;
                $cellsCount > $colCount and $colCount = $cellsCount;
                my $i = 0;
                my $id;
                foreach my $cell (@cells) {
                    my $value;

                    while (exists $rowSpans{$i} and $rowSpans{$i}{'-row'}-- > 0) {
                        # insert spanned cell from previous row(s)
                        $value = $rowSpans{$i}{'-value'};
                        $id //= $rowSpans{$i}{'-id'};

                        if (scalar @fields > $i) {
                            $data{$fields[$i++]} = $value;
                        } else {
                            $data{$i++} = $value;
                        }
                    }

                    $value = $cell->as_text_trimmed;
                    if ($cell->id) {
                        $id //= $cell->id;
                    }
                    if (defined $cell->attr('rowspan')) {
                        # remember spanned row values for next cells
                        $rowSpans{$i} = {
                            '-row' => $cell->attr('rowspan') - 1,
                            '-value' => $value,
                            '-id' => $id,
                        };
                    } else {
                        $rowSpans{$i}{'-row'} = 0;
                    }

                    if (scalar @fields > $i) {
                        $data{$fields[$i++]} = $value;
                    } else {
                        $data{$i++} = $value;
                    }
                }
                for (my $c = $i; $c < $colCount; $c++) {
                    if (exists $rowSpans{$c} and $rowSpans{$c}{'-row'}-- > 0) {
                        # insert spanned cell from previous row(s)
                        my $value = $rowSpans{$c}{'-value'};
                        $id //= $rowSpans{$c}{'-id'};

                        if (scalar @fields > $c) {
                            $data{$fields[$c]} = $value;
                        } else {
                            $data{$c} = $value;
                        }
                    }
                }

                my @columnNames = (keys %data);
                # need at least 2 extracted data columns (episode and name)
                unless (scalar(@columnNames) >= 2) {
                    #$isVerbose > 2 and
                        logger ('T',
                        sprintf(
                            "skipping record with too small columns count '%d':", scalar(@columnNames)
                        ),
                        map { sprintf('%-20s: %s, ', $_, $data{$_}) } keys %data
                    );
                    next;
                }

                if (@columnNames) {
                    #grep /titel|title/i, @columnNames or next;
                    $episodeColumn ||= getNoInSeries (@columnNames);
                    unless (defined($episodeColumn)) {
                        $isVerbose > 1 and logger('T', "no episode column found, skipping table: $libKey");
                        last ROWS;
                    }

                    $data{'-content'} = $key;
                    $data{'-library'} = $libKey;
                    $data{'-regex'} = getRegEx(%data);
                    $data{'-season'} = $season;
                    $data{'-episode'} = ($episodeColumn && exists $data{$episodeColumn}) ? $data{$episodeColumn} : '';
                    $data{'-se_key'} = getEpisodeKey ($data{'-season'}, $data{'-episode'});
                    $data{'-se_key'} =~ s#[^a-z0-9_]#_#ig;
                    ($data{'-year'} = $key) =~ s/\b$data{'-episode'}\b//; # remove episode (could conflict with year)
                    $data{'-year'} =~ s#^.*?((19|20|21)\d{2}).*?$#$1# or $data{'-year'} = ''; # extract year
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

                if ($useEpisodeOfYear > 0) {
                    my @seKeyList = sort { $db{$a}{'-se_key'} cmp $db{$b}{'-se_key'} } grep $_ =~ m/^[^-].*/, keys %db;
                    foreach my $key (@seKeyList) {
                        my $data = $db{$key};
                        exists $db{'-byYear'}{$data->{'-year'}} or $db{'-byYear'}{$data->{'-year'}} = 0;
                        $data->{'-year_episode'} = ++$db{'-byYear'}{$data->{'-year'}};
                        $data->{'-se_key_by_year'} = getEpisodeKey ($data->{'-year'}, $data->{'-year_episode'});
                        $data->{'-se_key_by_year'} =~ s#[^a-z0-9_]#_#ig;
                    }
                }
            }
        }

        $isVerbose > 1 and logger ('T', "library '$library' with: ${entries} entries");
        $isVerbose > 1 and logger ('T', "---------------------------------------------------------------------------------------------");
    }

    my @keys = sort keys %library;
    my $libraryTables = scalar @keys or logger ('F', "+++ no library entries found!!!");
    if ($newEntries > 0) {
        $library{'-libraryKeys'} = \@keys;
        my %fileHash = map {($_, 1)} @libraryFiles;
        $library{'-libraryFiles'} = \%fileHash;
        $isDumpLibrary and print STDERR Dumper(\%library);
    }
    $isVerbose > 1 and logger ('T', "library tables found: ${libraryTables} (in " . (scalar @libraryFiles) . " HTML files)");
}

sub checkFile #($fileName, $path)
{
    my ($fileName, $path) = @_;
    $isVerbose > 1 and logger ('T',
        '---------------------------------------------------------------------------------------------',
        "checking file: '${fileName}'"
    );

    $isAllFiles and $fileName =~ m/\.html$/ and return; # skip library files

    my $bn = basename($fileName);
    my $bnOrg = $bn;
    my $bn_enc = !Encode::is_utf8($bn) ? Encode::decode('utf-8', $bn) : $bn;
    my $seKeyPattern = qr/([_(]*S\d[^_-]*_E\d[^_-]*[_)]*)/;
    #TODO: Check if this could be used as fallback
    $bn_enc =~ s/$seKeyPattern//g; # ignore existing se-keys
    my $rc = 0;
    foreach my $libraryFile (@{$library{'-libraryKeys'}}) {
        $libraryFile =~ m/^-/ and next;
        my $db = $library{$libraryFile};

        foreach my $rowKey (@{$db->{'-rowKeys'}}) {
            $rowKey =~ m/^-/ and next;
            my $row = $db->{$rowKey};
            my $seKey = $useEpisodeOfYear ? $row->{'-se_key_by_year'} : $row->{'-se_key'};

            # match: (S01_E02_)?(Thema)(<search pattern from HTML [regex generator: getRegEx]>)
            my $pattern = "^(([^-]+)(?:-[^-]+)?(?:$row->{'-regex'}))";
            # append se-key as fallback
            (my $seKeyPattern = $seKey) =~ s/^.*S0*([^E_]+)_E0*([^_]+).*$/S0*$1_E0*$2/;
            $pattern .= "|.*?(${seKeyPattern})";
            my $re = qr/$pattern/ui;
            if ($bn_enc =~ $re) {
                (my $match = $1) //= '';
                (my $matchTopic = $2) //= '';
                (my $matchBySeKey = $3) //= '';

                $isVerbose > 1 and logger ('T',
                    "* matched '$bn'",
                    "  with pattern '$pattern'",
                    "  [match::'${match}'; matchTopic::'${matchTopic}'; matchSeKey::'${matchBySeKey}']",
                    "  from '${rowKey}'\@'${libraryFile}'"
                );

                #if ($bn =~ qr/^(S[^_]+_E[^_]+|$row->{'-se_key'})_/) {
                my @seMatchesBn = ($bn =~ m/$seKeyPattern/g);
                if ($bn =~ qr/^($seKey)_/ && scalar @seMatchesBn == 1) {
                    $isVerbose and logger ('T', "* matched '$bn' already contains series key '${seKey}', keeping current file name.");
                } else {
                    (my $newFileName = ${bn}) =~ s/$seKeyPattern//g; # remove old/wrong key from source file
                    $newFileName = "${seKey}_${newFileName}";
                    my $renRc = renameFileSet($bn, $newFileName, $path);
                    if ($renRc == 99) {
                        $row->{'-downloading'} = 1;
                    } else {
                        # use new name for stats
                        $bn = $newFileName;
                    }
                    $row->{'-renamed'}++;
                }

                if ($bn =~ $videoFileFilter) { # don't collect all files if $isAllFiles was set
                    $row->{'-matched'}++;
                    my $info;
                    if ($bn eq $bnOrg) {
                        $info = sprintf "%-60s", $bnOrg;
                    } else {
                        $info = sprintf "%-60s [new: %-60s]", $bnOrg, $bn;
                    }
                    $isVerbose > 0 and $info .= "  [match::'${match}'; matchTopic::'${matchTopic}'; matchSeKey::'${matchBySeKey}']";
                    if (exists $row->{'-matchedFile'}) {
                        push @{$row->{'-matchedFile'}}, ( $info );
                    }
                    else {
                        $row->{'-matchedFile'} = [ $info ];
                    }
                }

                $rc++;
                last;
            }
        }

        $rc and last;
    }
    $rc or logger ('W', "+++ no match for '$bn'");
}

my %seenDir;
my %multiDir;
sub walkDir #($item)
{
    my ($item) = @_;

    (my $globDir = $item) =~ s{\\}{/}g;
    $globDir =~ s/ /\\ /g;
    $globDir =~ s/'/\\'/g;
    foreach (glob("${globDir}/*")) {
        s/\\ / /g;
        s/\\'/'/g;
        if (-d $_) {
            (my $subDir = $_) =~ s/^$globDir//;
            if (exists $seenDir {$subDir}) {
                $isVerbose > 1 and print STDERR "\nTRACE  already seen folder: ".$seenDir {$subDir};
                exists $multiDir {$subDir} or $multiDir {$subDir} = $seenDir {$subDir};
                $multiDir {$subDir} .= "\n\t$_";
            }
            $seenDir {$subDir} = $_;
            $isVerbose > 1 and logger ('T', "scanning folder: '$_'", 
                "---------------------------------------------------------------------------------------------");
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
    my $ren = 0;

    my %filter = ('missing' => 0, 'seen' => 0, 'duplicate' => 0);

    foreach my $libraryFile (@{$library{'-libraryKeys'}}) {
        $libraryFile =~ m/^-/ and next;
        my $db = $library{$libraryFile};

        foreach my $rowKey (@{$db->{'-rowKeys'}}) {
            $rowKey =~ m/^-/ and next;
            my $row = $db->{$rowKey};

            my @info;
            foreach my $key (sort keys %$row) {
                $key =~ m/^-/ and next;
                push @info, sprintf("         %-40s: %s", $key, $row->{$key});
            }
            if (exists $row->{'-matched'}) {
                $row->{'-renamed'} and $ren++;
                $row->{'-downloading'} and $skip++;
                $seen++;
                if (defined $filterLibrary) {
                    $row->{'-content'} =~ $filterLibrary and ++$filter{'seen'};
                }
                if ($row->{'-matched'} > 1) {
                    $dup++;
                    if (defined $filterLibrary) {
                        $row->{'-content'} =~ $filterLibrary and ++$filter{'duplicate'} or next;
                    }
                    $isDuplicate or next;
                    logger ('E',
                        '---------------------------------------------------------------------------------------------',
                        'ERROR  duplicate found:',
                        @info,
                        '         Files:',
                        map { sprintf('         - %s', $_) } @{$row->{'-matchedFile'}}
                    );
                }
            } else {
                $mis++;
                if (defined $filterLibrary) {
                    $row->{'-content'} =~ $filterLibrary and ++$filter{'missing'} or next;
                }
                $isMissing or next;
                logger ('E',
                    '---------------------------------------------------------------------------------------------',
                    'ERROR  missing episode:',
                    @info
                );
            }
        }
    }

    logger ('I', "---------------------------------------------------------------------------------------------",
        "Summary:",
        "- episodes found:      " . sprintf("%10s", $seen, ) . ($skip ? sprintf(" / skipped: %10s (try again in about $downloadTimeout sec.)", $skip) : ''),
        "- episodes missing:    " . sprintf("%10s", $mis),
        "- duplicate matches:   " . sprintf("%10s", $dup),
        "- episodes renamed:    " . sprintf("%10s", $ren));
    if (defined $filterLibrary) {
        logger ('I', "---------------------------------------------------------------------------------------------",
            "- episodes found by '$filterLibrary':      " . sprintf("%10s", $filter{'seen'}),
            "- episodes missing by '$filterLibrary':    " . sprintf("%10s", $filter{'missing'}),
            "- duplicate matches by '$filterLibrary':   " . sprintf("%10s", $filter{'duplicate'}));
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
        "folder|f=s@"         => \@rootDirs,
        "summary|s!"          => \$isSummary,
        "show-missing|M!"     => \$isMissing,
        "show-duplicates|D!"  => \$isDuplicate,
        "dry-run|dr!"         => \$isDryRun,
        "keep-library|kl!"    => \$isKeepLibrary,
        "dump-library|dl!"    => \$isDumpLibrary,
        "verbose|v+"          => \$isVerbose,
        "help|h"              => \$isHelp,
        "usage|u"             => \$isUsage,
        "episode-filter|e=s"  => \$filterPattern,
        "episode-of-year|ey!" => \$useEpisodeOfYear,
        "scan-all!"           => \$isAllFiles,
    ) or pod2usage(-exitval => 9);
    $isUsage and pod2usage(-exitval => 8);
    $isHelp  and pod2usage(-exitval => 8, -verbose => 2);

    $filterPattern and $filterLibrary = qr/$filterPattern/i; #ex.: (M.*?nster|Thiel|Boerne)
    @rootDirs or pod2usage(
        -message => "+++ no folder provided!",
        -exitval => 9
    );

    $isVerbose > 1 and logger ('T', "---------------------------------------------------------------------------------------------",
        "Script:           $0",
        "OS:               $^O",
        "Perl:             $^X",
        "Includes:         ".join ("\n                  ", (grep {!m/^.$/} map { $^O =~ m/MSwin/i and s{/}{\\}g; $_; } @INC)),
        "Perl Version:     $^V ($])",
        "PID:              $$",
        "Root folders:     ".join ("\n                  ", @rootDirs));

    foreach my $rootDir (@rootDirs) {
        $rootDir =~ s#[/\\]*$##g;
        $newLibraryFound = 0;
        $isKeepLibrary or %library = ();
        logger('I', '---------------------------------------------------------------------------------------------',
            "scanning folder: '${rootDir}'",
            '---------------------------------------------------------------------------------------------');
        readLibrary (glob ("'$rootDir'/*.html"));
        walkDir ($rootDir);
        $isKeepLibrary and $newLibraryFound and logger ('I', "keeping library from folder: '${rootDir}'");
    }
    #$isVerbose > 1 and logger ('T', "---------------------------------------------------------------------------------------------");
    $isSummary and printMissingSeries();
    logger ('I', "duration ".(time () - $startTime)." sec.");
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
    --episode-of-year,-ey   count episode by year (S<year>_E<episode by year>_)
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