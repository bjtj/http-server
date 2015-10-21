#!/bin/bash

BASE=$PWD
DIR_BUILD=$PWD/build
DIR_WORLD=$PWD/world

OPT=debug-install

if [ -n "$1" ]; then
	OPT=$1
fi

clean() {
	rm -rf $DIR_BUILD $DIR_WORLD
}

reconf() {
	autoreconf -i
}

config_debug() {
	clean
	mkdir -p $DIR_BUILD
	mkdir -p $DIR_WORLD
	cd $DIR_BUILD
	$BASE/configure --prefix "$DIR_WORLD"
}

config_install() {
	clean
	mkdir -p $DIR_BUILD
	cd $DIR_BUILD
	$BASE/configure
}

debug_install() {
	cd $DIR_BUILD
	make && make install
}

install() {
	cd $DIR_BUILD
	make && sudo make install
}

case $OPT in
	reconf)
		reconf
		;;
	config-debug)
		config_debug
		;;
	config-install)
		config_install
		;;
	debug-install)
		debug_install
		;;
	install)
		install
		;;
	dist)
		cd $DIR_BUILD && make dist
		;;
	clean)
		clean
		;;
	*)
		echo "unknown option - $OPT"
		exit 1
		;;
esac
