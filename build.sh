#!/bin/bash

BASE=$PWD
DIR_BUILD=$PWD/build
DIR_WORLD=$PWD/world

OPT=build

if [ -n "$1" ]; then
	OPT=$1
fi

case $OPT in
	build)
		autoreconf -i
		RET=$?
		if [ "$RET" != 0 ]; then
			echo "autoreconf failed..."
			exit 1
		fi
		
		mkdir -p $DIR_BUILD
		mkdir -p $DIR_WORLD
		
		cd $DIR_BUILD

		$BASE/configure LDFLAGS=-L/usr/local/lib CPPFLAGS=-I/usr/local/include --prefix "$DIR_WORLD" && make && make install
		;;
	make)
		cd $DIR_BUILD && make && make install
		;;
	dist)
		cd $DIR_BUILD && make dist
		;;
	clean)
		rm -rf $DIR_BUILD $DIR_WORLD
		;;
	*)
		echo "unknown option - $OPT"
		exit 1
		;;
esac
