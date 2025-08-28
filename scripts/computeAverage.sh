#!/bin/sh

yourfilenames=`ls *.txt`
badcount=0
count=0
sum=0
for eachfile in $yourfilenames
do
	badcount=`expr $badcount + 1`
	value=$( tail -n 1 $eachfile )
	if [ $value != Nil ]
	then
		sum=$(awk "BEGIN {print $sum+$value; exit}")
		#sum=`expr $sum + $value`
		count=`expr $count + 1`
		#echo $(awk "BEGIN {print $sum/$count; exit}")
	fi 
done
echo $(awk "BEGIN {print $sum/$count; exit}")
#echo $(awk "BEGIN {print $sum/$badcount; exit}")