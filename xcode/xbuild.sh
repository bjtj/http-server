#!/bin/bash

# https://developer.apple.com/library/ios/technotes/tn2339/_index.html

OPT=build

if [ -n "$1" ]; then
	OPT=$1
fi

cd http-server-console
case $OPT in
	build)
		xcodebuild -scheme http-server-cocoa build
		;;
	build-release)
		xcodebuild -scheme http-server-console -configuration "Release" build
		;;
	build-debug)
		xcodebuild -scheme http-server-console -configuration "Debug" build
		;;
	list)
		xcodebuild -list -project http-server-console.xcodeproj
		;;
	*)
		echo "error: unknown option - '$OPT'"
		;;
esac


