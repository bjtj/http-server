#!/bin/bash

PATH=$PWD/world/bin:$PATH

OPT=run

if [ -n "$1" ]; then
	OPT=$1
	shift
fi

case $OPT in
	run)
		http-server $@
		;;
	gdb)
		gdb --args http-server $@
		;;
	check)
		valgrind --leak-check=yes http-server $@
		;;
	*)
		echo "unknown option -- $OPT"
		exit 1
		;;
esac
