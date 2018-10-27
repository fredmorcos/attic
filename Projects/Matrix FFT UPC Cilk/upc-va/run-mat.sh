#!/bin/bash

methods="BULK_COPY"
grep_exp="^icc|^upcc|^UPCR|^GASNET"

# veclen=100000
# showvec=no

veclen=10
showvec=yes

# blocksizes="1 VEC_LEN/NT \(VEC_LEN/NT\)+1"
# threads="20 23"

# blocksizes="1 VEC_LEN/NT"
blocksizes="1"
threads=2

function run
{
    make clean > /dev/null;
    echo "=== $m threads=$1 BS=$2 ==="
    make run-matrix-addition UPC_THREADS=$1 VEC_LEN=$veclen \
	SHOW_VEC=$showvec DO_CHECK=yes METHOD=$m BS=$2 | grep -vPe $grep_exp
    echo ""
}

for m in $methods
do
    for bs in $blocksizes
    do
	for t in $threads
	do
	    run $t $bs
	done
    done
done
