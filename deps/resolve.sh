#!/bin/bash

BASE=$PWD
DEPS=`ls`

for ITEM in $DEPS; do
	TARGET="$BASE/$ITEM"
	if [ -d "$TARGET" ]; then
		cd "$TARGET"
		git submodule init
		git submodule update
	fi
done
