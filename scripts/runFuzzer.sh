#!/bin/sh
flag=$1
cd ..
yourfilenames=`ls testprograms/*.swift`
for eachfile in $yourfilenames
do
	#echo $eachfile
	java -jar target/scala-2.12/FuzzingSwift-assembly-1.0.jar $eachfile -f $flag
done