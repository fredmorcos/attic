#!/bin/sh

export PYTHONPATH=$PYTHONPATH:`pwd`:grafer:grafer/engine/:grafer/ui/:grafer/data/
python grafer/ui/Main.py
