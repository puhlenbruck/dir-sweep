# Directory Sweeper (dir-sweep)

A Utility to clean out old files from directories

## Installation

Binary distributions are not provided at this time, you misu build from source

### Build from source

dir-sweep is built using [stack](http://haskellstack.org)

Clone this repository and install using stack

```shell
git clone https://github.com/puhlenbruck/dir-sweep.git

cd dir-sweep

stack install
```

Stack will build the executable and place it in `~/.local/bin` (`%APPDATA%/local/bin` on windows).

## Usage

For a list of the command line options, run
```shell
dir-sweep -h
```


### Basic usage 

To run, pass in a time window and a directory to clean

```shell
dir-sweep -t 3d tmp
```

This will delete any file of directory older than 3 days in the directory 'tmp'.

The time window (`-t|--time`) is given in the form `<number><unit>`.  The valaiable units are:
- `s` (seconds)
- `mi` (minutes)
- `h` (hours)
- `d` (days)
- `w` (weeks)

Examples: 
- `90s` (90 seconds)
- `180d` (180 days)
- `2w` (2 weeks)

If no time window is provided then no files are deleted.

Multiple target directories can be specified and each will be processed separately.  **Warning: passing multiple directories in the same directory tree could have unexpected results**

### Sub-Directories

Sub-directories within the target directory are treated as a single file.  The entire directory will be recursivly deleted if it's modify time is older than the threshold.  This behaviour can be controlled with the `--sub-dir` option.

The available options for `--sub-dir` are:
- FILE (default) The entire directory is treated as a single file.  It will either be deleted entirely, or not at all.  It counts as only a single file to the deletion limits ((see below)[#Limits])
- IGNORE Directories are ignored.  Only files will be deleted and counted in limits ((see below)[#Limits])
- RECURSIVE Each file in a directory (and all it's sub-directories) will be considered individually against the threshold and limits ((see below)[#Limits]).  The directories themselves will not be deleted or counted.


### Limits
You can also place limits on the number of files to be deleted:

```shell
dir-sweep --min-keep 5 -t 3d tmp
```
Will keep the 5 most recent files, even if they are older than the threshold.

```shell
dir-sweep --max-keep 5 -t 3d tmp
```

Will keep at most 5 files, even if there are more files younger than the threshold.

`--max-keep` can be used without a time threshold.  In this case n youngest files will be kept and the rest of the files deleted.

`--min-keep` and `--max-keep` can be combined.

### Dry-Run

dir-sweep supports a dry-run mode:

```shell
dir-sweep -d -t 3d tmp
```

When the dry-run flag is set, No files will be deleted.  Instead the files that would be deleted are listed on stdout.