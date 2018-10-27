#!/bin/sh

find . -iname "main" | xargs rm -f
find . -iname "*.trans.*" | xargs rm -f