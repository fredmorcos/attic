#!/bin/bash

NETWORK=ibv
UPC_THREADS=6
UPC_HEAP=1G
THREADS_PER_NODE=1

export GASNET_SSH_SERVERS="$(hvlt_fastest $UPC_THREADS $THREADS_PER_NODE)"
export UPC_SHARED_HEAP_SIZE=$UPC_HEAP

BINS=$(ls -1 *.upc | sed -e 's/\.upc//g')

for b in $BINS;
do
    if [[ "$1" != "noclean" ]]; then
	echo "cleaning $b..."
	rm -f $b
    fi

    if [[ "$1" != "clean" ]]; then
	echo "building $b..."
	make UPC_THREADS=$UPC_THREADS NETWORK=$NETWORK $b

	echo "running $b..."
	upcrun -network=$NETWORK -c 1 -n $UPC_THREADS -shared-heap=$UPC_HEAP ./$b
    fi
done
