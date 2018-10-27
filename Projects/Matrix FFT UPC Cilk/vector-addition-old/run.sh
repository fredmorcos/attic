#!/bin/bash

BINS=$(ls -1 *.upc | sed -e 's/\.upc//g')

if [[ "$1" == "clean" ]]; then
    for b in $BINS;
    do
	echo "cleaning $b..."
	rm -f $b
    done

    exit
fi

./run.sh clean

VECLEN=2000000
NETWORK=ibv
UPC_THREADS=16
UPC_HEAP=500M
THREADS_PER_NODE=1

export GASNET_SSH_SERVERS="$(hvlt_fastest $UPC_THREADS $THREADS_PER_NODE)"
# export GASNET_SSH_SERVERS="$(hvlt_fastest_sort --nodes $UPC_THREADS)"
export UPC_SHARED_HEAP_SIZE=$UPC_HEAP

echo "GASNet SSH Servers: $GASNET_SSH_SERVERS"

for b in $BINS;
do
    echo "building $b..."

    make UPC_THREADS=$UPC_THREADS NETWORK=$NETWORK VECLEN=$VECLEN $b

    if [[ "$?" != "0" ]]; then
	exit 1
    fi

    echo "running $b..."
    UPC_RUN_CMD="upcrun -c $THREADS_PER_NODE -N $UPC_THREADS -shared-heap=$UPC_HEAP ./$b"
    echo $UPC_RUN_CMD
    time $UPC_RUN_CMD
done
