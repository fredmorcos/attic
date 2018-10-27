#!/bin/sh

aclocal && autoheader && touch stamp-h && aclocal && autoconf && automake --add-missing && automake

