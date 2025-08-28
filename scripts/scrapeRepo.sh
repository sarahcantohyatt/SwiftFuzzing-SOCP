#!/bin/bash

cd ..
yourfilenames=`find repos -type f -name '*.swift'`
echo $yourfilenames
for eachfile in $yourfilenames
do
	echo $eachfile
	swiftc -o $eachfile.executable $eachfile > $eachfile.output 2>&1
	if [ ! -f $eachfile.executable ]
	then
		rm $eachfile.output
	else
		cp $eachfile testprograms
		rm $eachfile.executable
		rm $eachfile.output
	fi
done
