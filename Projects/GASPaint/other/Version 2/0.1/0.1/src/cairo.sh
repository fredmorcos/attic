#!/bin/bash

if [ ! -e config.h ] then
	cp config.temp config.h
fi
echo "#undef __OPENGL" >> config.h
echo "#define __CAIRO" >> config.h
echo "#endif" >> config.h

