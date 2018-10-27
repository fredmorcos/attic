#!/bin/bash

# methods="SINGLE_THREAD LOCAL_BLOCKS FORALL_AFFINITY FORALL_AFFINITY_SPLIT FORALL_ITERATION FORALL_ITERATION_SPLIT FORALL_PARTITIONED FORALL_PARTITIONED_SPLIT FOR_SINGLE_ELEMENT_BLOCKS FOR_BLOCKS_TRAVERSAL FOR_BLOCKS_TRAVERSAL_SPLIT FOR_PARTITIONED FOR_PARTITIONED_SPLIT SINGLE_THREAD_BULK_COPY SINGLE_THREAD_BULK_COPY_SPLIT SINGLE_THREAD_BULK_COPY_ASYNC SINGLE_THREAD_BULK_COPY_ASYNC_SPLIT"

methods="FORALL_PARTITIONED FORALL_PARTITIONED_SPLIT"

grep_exp="^icc|^upcc|^UPCR|^GASNET"

veclen=1000000
showvec=no

blocksizes="1 2 \(VEC_LEN/NT\)"
threads=4

function run
{
    echo "=== $m threads=$1 BS=$2 ==="

    make clean &>/dev/null;

    make main.upc.trans.c UPC_THREADS=$1 VEC_LEN=$veclen \
    	SHOW_VEC=$showvec DO_CHECK=yes METHOD=$3 BS=$2 | grep -vPe $grep_exp

    filename=$3_${2/\//_}.c
    filename=${filename/(/}
    filename=${filename/)/}
    filename=${filename/\\/}
    filename=${filename/\\/}

    mv main.upc.trans.c $filename

    make run-vector-addition UPC_THREADS=$1 VEC_LEN=$veclen \
    	SHOW_VEC=$showvec DO_CHECK=yes METHOD=$3 BS=$2 | grep -vPe $grep_exp

    echo ""
}

for m in $methods
do
    for bs in $blocksizes
    do
	for t in $threads
	do
	    run $t $bs $m
	done
    done
done
