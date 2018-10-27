#!/bin/bash

while getopts "e:d:k:x:vh" OPT; do
    case $OPT in
	e)
	    echo "enable $OPTARG"
	    ;;
	d)
	    echo "disable $OPTARG"
	    ;;
	k)
	    echo "keep $OPTARG"
	    ;;
	x)
	    echo "except $OPTARG"
	    ;;
	v)
	    echo "verbose"
	    ;;
	h)
	    echo "help"
	    ;;
	?)
	    echo "not sure what you just passed"
	    ;;
	*)
	    echo "something else completely"
	    ;;
    esac
done
