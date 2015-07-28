#!/bin/bash

BASE=$PWD
DIR_BUILD=$PWD/build
DIR_WORLD=$PWD/world

OPT=build

if [ -n "$1" ]; then
	OPT=$1
fi

reconf() {
	autoreconf -i
}

build() {
	mkdir -p $DIR_BUILD
	mkdir -p $DIR_WORLD
	cd $DIR_BUILD
	$BASE/configure --prefix "$DIR_WORLD" && make && make install
}

install() {
	cd $DIR_BUILD
	$BASE/configure && make && sudo make install
}

case $OPT in
	reconf)
		reconf
		;;
	build)
		build
		;;
	all)
		prepare
		build
		;;
	make)
		cd $DIR_BUILD && make && make install
		;;
	install)
		install
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
