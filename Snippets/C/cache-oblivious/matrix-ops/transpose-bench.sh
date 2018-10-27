#!/bin/sh

make clean
make

MATSIZE=10000

echo "=== GCC TRANSPOSE BENCHMARK ==="

for ((i=16; i<=256; i*=2))
do
    ./testbench-gcc --leaf-size $i --matrix-size $MATSIZE --transpose
    echo ""
done

echo "=== END GCC TRANSPOSE BENCHMARK ==="

echo "=== PSCC TRANSPOSE BENCHMARK ==="

for ((i=16; i<=256; i*=2))
do
    ./testbench-pscc --leaf-size $i --matrix-size $MATSIZE --transpose
    echo ""
done

echo "=== END PSCC TRANSPOSE BENCHMARK ==="

echo "=== CLANG TRANSPOSE BENCHMARK ==="

for ((i=16; i<=256; i*=2))
do
    ./testbench-clang --leaf-size $i --matrix-size $MATSIZE --transpose
    echo ""
done

echo "=== END CLANG TRANSPOSE BENCHMARK ==="

make clean