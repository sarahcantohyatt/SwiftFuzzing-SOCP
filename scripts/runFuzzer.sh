#!/bin/sh
flag=$1
#copy the seed files first
cd ..
mkdir testprograms
cd swift-programs
filenames=`ls *.swift`
cd ..
for file in $filenames
do
	cp swift-programs/$file testprograms/$file
done

yourfilenames=`ls testprograms/*.swift`
for eachfile in $yourfilenames
do
	#echo $eachfile
	java -jar target/scala-2.12/FuzzingSwift-assembly-1.0.jar $eachfile -f $flag
done