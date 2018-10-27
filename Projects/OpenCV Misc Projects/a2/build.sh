#!/bin/sh

rm -rf build
mkdir build
cd build
cmake ..
make install
cd ..

