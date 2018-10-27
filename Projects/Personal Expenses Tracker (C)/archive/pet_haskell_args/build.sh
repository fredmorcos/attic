#!/bin/sh

SRC=Main.hs
BIN=pet

if [ -z $1 ]; then
    echo "Build mode"
elif [ $1 = "clean" ]; then
    CLEANCMD="rm -rf *.hi *.o $BIN.prof $BIN Extra/*.hi Extra/*.o"
    echo "Clean command: $CLEANCMD"
    $CLEANCMD
    exit $?
elif [ $1 = "test" ]; then
    echo "Test mode"
    ./build || exit 1
    for i in tests/*; do
        echo "==== $i"
        ./$BIN $i
        echo "==== END"
    done
    exit 0
fi

if [ -z $HC ];     then HC=ghc ; fi                  # Haskell compiler
if [ -z $HFLAGS ]; then HFLAGS="-Wall $HFLAGS" ; fi  # Haskell compiler flags

if [ -z $PROF ]; then PROF=no ; fi
if [ -z $TRD ];  then TRD=no  ; fi
if [ -z $OPT ];  then OPT=no  ; fi

set nocasematch

case $PROF in
    1 | yes | y ) PROF=yes && HFLAGS="-prof $HFLAGS" ;;
    0 | no  | n ) PROF=no ;;
    * ) echo -e "Invalid value for PROF (profiling)\n  Set yes or no" && \
        exit 1 ;;
esac

case $TRD in
    1 | yes | y ) TRD=yes && HFLAGS="-threaded $HFLAGS" ;;
    0 | no  | n ) TRD=no ;;
    * ) echo -e "Invalid value for TRD (threading)\n  Set yes or no" && \
        exit 1 ;;
esac

case $OPT in
    1 | yes | y ) OPT=yes && HFLAGS="-O $HFLAGS" ;;
    0 | no  | n ) OPT=no ;;
    * ) echo -e "Invalid value for OPT (optimization)\n  Set yes or no" && \
        exit 1 ;;
esac

echo "Profiling enabled:    $PROF"
echo "Threading enabled:    $TRD"
echo "Optimization enabled: $OPT"

echo "Haskell compiler:     $HC"
echo "Compiler flags:       $HFLAGS"

HCMD="$HC $HFLAGS $SRC --make -o $BIN"

echo "Build command:        $HCMD"
$HCMD

exit $?
