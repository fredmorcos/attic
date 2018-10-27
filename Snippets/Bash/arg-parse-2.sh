#!/bin/sh

# global info
VERBOSE=
IFILE=
OFILE=

READIFILE=0
READOFILE=0

perr() {
    echo "$1" >&2
}

plog() {
    if [ "$VERBOSE" ]; then
	echo "[LOG] $1" >&2
    fi
}

pwarn() {
    echo "[WRN] $1" >&2
}

pversion() {
    perr "$0: Bash Argument Parser"
}

phelp() {
    pversion
    perr ""
    perr "Usage: $0 [arguments]"
    perr ""
    perr "Arguments"
    perr "  -h,--help            Show this help"
    perr "  -V,--version         Show version information"
    perr "  -v,--verbose         Verbose output"
    perr "  -i,--input [FILE]    Specify input file"
    perr "  -o,--output [FILE]   Specify output file"
    perr ""
    perr "Note: order of arguments is sensitive"
}

# main entry
if [ "$#" -eq 0 ]; then
    perr "Error: No arguments given."
    perr " -> See: $0 --help"
    exit 1
fi

for arg in $@; do
    if [ "$READIFILE" -eq 1 ]; then
	IFILE="$arg"
	READIFILE=0
    elif [ "$READOFILE" -eq 1 ]; then
	OFILE="$arg"
	READOFILE=0
    elif [ "$arg" == "-h" -o "$arg" == "--help" ]; then
	phelp
	exit 0
    elif [ "$arg" == "-V" -o "$arg" == "--version" ]; then
	pversion
	exit 0
    elif [ "$arg" == "-v" -o "$arg" == "--verbose" ]; then
	VERBOSE=1
    elif [ "$arg" == "-i" -o "$arg" == "--input" ]; then
	READIFILE=1
    elif [ "$arg" == "-o" -o "$arg" == "--output" ]; then
	READOFILE=1
    fi
done

plog "Parameters"
plog " -> Verbose=$VERBOSE"
plog " -> Input File=$IFILE"
plog " -> Output File=$OFILE"
