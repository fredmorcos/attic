#!/bin/bash

if [ ! -f config.h ] then
	cp config.temp config.h
fi
echo "#undef __CAIRO" >> config.h
echo "#define __OPENGL" >> config.h
echo "#endif" >> config.h

