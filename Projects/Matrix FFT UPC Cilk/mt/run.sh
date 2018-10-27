#!/bin/bash

# The following is a mapping of matrix block grid to machine nodes
# such that adjacent blocks (in terms of communication) have a direct
# IB connection, i.e. 2GB/s bandwidth on Alex.

NODES_1="r1i1n0"

NODES_2="r1i0n0 r1i0n1\
         r1i0n2 r1i0n3"

NODES_3="r1i0n0 r1i0n1 r1i0n2\
         r1i0n3 r1i0n4 r1i0n5\
         r1i0n6 r1i0n7 r1i0n8"

NODES_4="r1i3n0  r1i0n0  r1i0n2  r1i0n4\
         r1i0n1  r1i3n1  r1i0n6  r1i0n8\
         r1i0n3  r1i0n7  r1i3n2 r1i0n10\
         r1i0n5  r1i0n9 r1i0n11 r1i0n12"

NODES_5="r1i3n0 r1i0n0  r1i0n2  r1i0n4  r1i0n6\
         r1i0n1 r1i3n1  r1i0n8  r1i0n10 r1i0n12\
         r1i0n3 r1i0n9  r1i3n2  r1i0n14 r1i1n0\
         r1i0n5 r1i0n11 r1i0n15 r1i3n3  r1i1n2\
         r1i0n7 r1i0n13 r1i1n1 r1i1n3  r1i1n4"

NODES_6="r1i3n0 r1i0n0  r1i0n2  r1i0n4  r1i0n6  r1i0n8\
         r1i0n1 r1i3n1  r1i0n10 r1i0n12 r1i0n14 r1i1n0\
         r1i0n3 r1i0n11 r1i3n2  r1i1n2  r1i1n4  r1i1n6\
         r1i0n5 r1i0n13 r1i1n3  r1i1n14 r1i1n8  r1i1n10\
         r1i0n7 r1i0n15 r1i1n5  r1i1n9  r1i1n15 r1i1n12\
         r1i0n9 r1i1n1  r1i1n7  r1i1n11 r1i1n13 r1i3n3"

# We can alternatively select nodes dynamically using machine tools.
# export GASNET_SSH_SERVERS=`hvlt_nodelist -n $nd -p $t -m w `
# export MPINODES=`hvlt_nodelist -n $nd -p $t  `

# Max PSQRT, Alex has 48 nodes total -> floor(psqrt(48)).
# We can only use 6x6=36 nodes from Alex for the square matrices.
# If Alex gets an extra node, we can use 7x7=49 nodes.
MAX_NODES_FACTOR=6
ALEX_TOTAL_NODES=48

# 1x 2x 3x 1/2x 1% of memory.
BUFFER_SIZES="94387383 62924922 115731231 3146246"

# List of blocksizes per node. Max for UPC about 70K, 23120 for MKL.
MAXK="2317"

QUIET_PARAMS=''
HEAP_SIZE=8G

function execute_bench () {
    resultname=`date +%s`

    for mk in $MAXK; do
	for max_nodes_factor in $(eval echo {$MAX_NODES_FACTOR..0}); do
	    for buffer_size in $BUFFER_SIZES; do
		let upc_threads=$max_nodes_factor*$max_nodes_factor

		export GASNET_SSH_SERVERS=$(eval echo \$NODES_$max_nodes_factor)
		# export GASNET_SSH_SERVERS=`hvlt_nodelist -n $nd -p $t -m w`

		for (( cilk_threads=8; ct>=8; ct/=2 )); do
		    make clean
		    make UPC_THREADS=$upc_threads HEAP_SIZE=$HEAP_SIZE \
			 MAX_NODES_FACTOR=$max_nodes_factor \
			 BUFFER_SIZE=$buffer_size LS=7 \
			 PN=$pn NODES=$nd QUIET=$quiet \
			 CILK_THREADS=$cilk_threads \
			$1
		    export CILK_NWORKERS=$ct
		    export UPC_CILK_NWORKERS=$ct
		    upcrun -network=ibv -shared-heap=$hs -c $pn -n $t $1 &> res/$1-$resultname.txt
		done
	    done
	done
    done
}

case "$1" in
    test_comp)
	test_compilation
	;;
    *)
	execute_bench mtc+
	execute_bench mt2c+
	execute_bench mt3c+
esac
