#!/bin/sh

[[ $CC ]] || export CC=gcc
[[ $CFLAGS ]] || export CFLAGS="-pedantic -Wall -Werror -std=c99"
[[ $LDFLAGS ]] || export LDFLAGS="-Wl,--hash-style=gnu -Wl,--as-needed"

export FLEX=flex
export BISON=bison

SUB_PRJS=hllc

if [ "$1" == "debug" ]; then
    export CFLAGS="$CFLAGS -g -DDEBUG"
elif [ "$1" == "clean" ]; then
    echo "=== cleaning ==="
    for i in $SUB_PRJS
    do
	echo "=== cleaning $i ==="
	cd $i
	./build.sh clean
    
	if [ $? -ne 0 ]; then
	    echo "=== error cleaning $i ==="
	    exit 1
	else
	    echo "=== finished cleaning $i ==="
	fi
    done
    echo "=== done cleaning ==="
    exit 0
fi

echo "CFLAGS = $CFLAGS"
echo "LDFLAGS = $LDFLAGS"

for i in $SUB_PRJS
do
    echo "=== building $i ==="
    cd $i
    ./build.sh
    
    if [ $? -ne 0 ]; then
	echo "=== error building $i ==="
	exit 1
    else
	echo "=== finished building $i ==="
    fi
done

cd ..
echo "=== done ==="

exit 0
