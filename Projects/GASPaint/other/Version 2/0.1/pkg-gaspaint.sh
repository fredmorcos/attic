#!/bin/bash

SRC="src/*.c src/makefile src/*.h"
DOCS="docs/ISSUES docs/FORMAT"
DIR=trunk
NAME=gaspaint
FILES="LICENSE COPYING makefile"
VER=0.1

mkdir -p $NAME-$VER/src &&
mkdir -p $NAME-$VER/docs &&
cd $DIR &&
cp -r $FILES ../$NAME-$VER/. &&
cp -r $SRC ../$NAME-$VER/src/. &&
cp -r $DOCS ../$NAME-$VER/docs/. &&
cd .. &&

tar -cvjf $NAME-$VER.tar.bz2 $NAME-$VER/ &&
mv $NAME-$VER.tar.bz2 tags/. &&
rm -rf $NAME-$VER/
