#!/bin/sh

cd ..
yourfilenames=`ls testprograms/*.swift`
count=0
for eachfile in $yourfilenames
do
	#echo $eachfile
	java -jar target/scala-2.12/FuzzingSwift-assembly-1.0.jar $eachfile -c #> scripts/$count.txt
	count=`expr $count + 1`
done