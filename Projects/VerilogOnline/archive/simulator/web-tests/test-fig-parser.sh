#!/bin/sh

DATA=`cat $1 | ../fig/parser.py | ../fig/raphael.py`

echo "machine = $DATA"
