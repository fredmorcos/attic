#!/bin/bash

# This script will print out the time, date, logged-in users,
# system uptime and save them to info.txt

# DATA=""

# DATA=${DATA}"Date: `date +%D`"$'\n'
# DATA=${DATA}"Time: `date +%T`"$'\n'
# DATA=${DATA}"Users: `users`"$'\n'
# DATA=${DATA}"Uptime: `uptime`"$'\n'

# cat /dev/null > info.txt
# echo $DATA
# echo $DATA >> info.txt

DATA=""
cat /dev/null > info.txt

DATA="Date: `date +%D`"
echo $DATA
echo $DATA >> info.txt
DATA="Time: `date +%T`"
echo $DATA
echo $DATA >> info.txt
DATA="Users: `users`"
echo $DATA
echo $DATA >> info.txt
DATA="Uptime: `uptime`"
echo $DATA
echo $DATA >> info.txt

exit 0
