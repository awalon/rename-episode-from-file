# TV Series Renamer (beta)

[![Poject: Rename Episode](https://img.shields.io/badge/Project-rename--episode--from--file-red.svg?style=flat-square)](https://github.com/awalon/rename-episode-from-file/)
[![License: GPL](https://img.shields.io/badge/License-GPL-green?style=flat-square)](LICENSE.md)
[![GitHub issues](https://img.shields.io/github/issues/awalon/rename-episode-from-file?style=flat-square)](https://github.com/awalon/rename-episode-from-file/issues)
![Perl version: 5.32](https://img.shields.io/badge/Version-5.32-informational?style=flat-square&logo=perl)
[![GitHub forks](https://img.shields.io/github/forks/awalon/rename-episode-from-file?style=flat-square)](https://github.com/awalon/rename-episode-from-file/network)
[![GitHub stars](https://img.shields.io/github/stars/awalon/rename-episode-from-file?style=flat-square)](https://github.com/awalon/rename-episode-from-file/stargazers)

## Motivation
Recordings of TV series created with [MediathekView](https://mediathekview.de/) was not sorted
as expected if viewed within [Plex Media Server](https://www.plex.tv/).

## License

100% FREE under [GPL](LICENSE.md) license

## Configuration

### MediathekView
File name pattern: `%t-%T-%z`

### Plex Media Server

Mediathek with:

* 

### Rename Tool 
Download (rename-episode-from-file.pl) from Github:
```shell
wget rename-episode-from-file.pl
```
## Execution

1. Open Wikipedia
2. Search for Episode List, ex.: [Tatort - Episodes](https://de.wikipedia.org/wiki/Liste_der_Tatort-Folgen)
3. Save unchanged Episode List into folder containing recordings
4. Execute (rename-episode-from-file.pl), with dry-run option: 
   ```shell
   rename-episode-from-file.pl --dry-run --folder <path to your Episode specific recordings>
   ```
5. If output of dry-run was as expected, execute (rename-episode-from-file.pl) to rename your files:
   ```shell
   rename-episode-from-file.pl --folder <path to your Episode specific recordings>
   ```

## Frequently asked Questions / FAQs

### How rename newly added recordings
Just execute (rename-episode-from-file.pl) again, as files already having prefix 
S&lt;season&gt;_E&lt;episode&gt;_ won't be changed.

### Some files of TV series set wasn't renamed
Just execute (rename-episode-from-file.pl) with `--dry-run --scan-all` to check if problem can
be automatically be fixed.
