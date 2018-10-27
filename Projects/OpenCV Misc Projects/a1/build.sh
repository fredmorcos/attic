#!/bin/sh

mkdir build
gtk-builder-convert ui.glade ui.xml
cd build
cmake ..
make
mv ../ui.xml .

