#!/bin/bash

# to build: make (-n) T=2

# export GASNET_SSH_SERVERS=`hvlt_nodelist -n 2 -p 2`
# echo $GASTNET_SSH_SERVERS

export GASNET_SSH_SERVERS="r1i3n15 r1i3n11"
upcrun -network=ibv -c 1 -n 2 ./test 
