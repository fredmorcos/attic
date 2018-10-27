#!/bin/sh

methods="space-partition space-partition-forall-toy"
grep_exp="^succ|^err|^lar_swap|^lar_time|^avg_time|^tot_swap|^avg_swap"

for m in $methods
do
    make clean > /dev/null;
    echo "=== $m ==="
    make run-transpose UPC_THREADS=20 MAT_W=1000 MAT_H=1000 ILS=BS JLS=BS GILS=32 GJLS=32 \
	SHOW_MAT=no DO_CHECK=yes METHOD=$m BS=MAT_W/NT | grep -Pe $grep_exp
    echo ""
done

# for m in $methods
# do
#     echo "=== $m ==="
#     make clean > /dev/null;
#     make run-transpose UPC_THREADS=20 MAT_W=1000 MAT_H=1000 ILS=BS JLS=BS GILS=64 GJLS=64 \
# 	SHOW_MAT=no DO_CHECK=yes METHOD=$m BS=1 | grep -Pe $grep_exp
#     echo ""
# done
