#!/bin/bash

BASE=$PWD
DIR_BUILD=$PWD/build
DIR_WORLD=$PWD/world

mkdir $DIR_BUILD
mkdir $DIR_WORLD

cd $DIR_BUILD

$BASE/configure --prefix "$DIR_WORLD" && make && make install
