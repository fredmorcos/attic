#!/bin/bash

NAME=heksniff
FILES="$NAME/LICENSE $NAME/COPYING $NAME/src $NAME/makefile"
VER=0.1

mkdir $NAME-$VER &&
cp -r $FILES $NAME-$VER/. &&

tar -cvjf $NAME-$VER.tar.bz2 $NAME-$VER/
rm -rf $NAME-$VER/
