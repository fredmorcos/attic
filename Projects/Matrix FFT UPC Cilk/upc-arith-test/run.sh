#!/bin/bash

function run
{
    make clean &>/dev/null;

    make run-arith-test UPC_THREADS=4 

}

run 

