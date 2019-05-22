#!/bin/sh

set -o posix
set -o errexit
set -o nounset
set -o pipefail

for input in $(echo tests.in/*); do
  for bin in ./mapred           \
             ./mapred_debug     \
             ./mapred_ptrcheck  \
             ./mapred_san       \
             ./mapred_sanaddr   \
             ./mapred_sanint    \
             ./mapred_sanleak   \
             ./mapred_sanmem    \
             ./mapred_santhread \
             "valgrind ./mapred_debug";
  do
    output=tests.out/$(basename $input)
    temp=$(mktemp)
    echo ">>> $bin $input ($temp)..."
    $bin $input 10 > $temp

    if ! /usr/bin/diff $output $temp &>/dev/null; then
      echo "$output != $temp"
      exit 1
    else
      rm $temp
    fi
  done
done
