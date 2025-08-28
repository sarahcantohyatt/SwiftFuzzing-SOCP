#!/bin/sh

cd ../testprograms
filename=`ls` #get the name of the file
cd ../scripts
echo "starting file: $filename"
timeout -s SIGINT 5 ./run_fuzzer_get_inputs.sh -1
exitstatus=$?
numfiles1=`zipinfo -t output.zip | head -n1 | cut -d " " -f1`
if [[ $exitstatus == 124 ]]; then
	numfiles1+=" TO"
fi
timeout -s SIGINT 10 ./run_fuzzer_get_inputs.sh -2
exitstatus=$?
numfiles2=`zipinfo -t output.zip | head -n1 | cut -d " " -f1`
if [[ $exitstatus == 124 ]]; then
	numfiles2+=" TO"
fi
timeout -s SIGINT 15 ./run_fuzzer_get_inputs.sh -3
exitstatus=$?
numfiles3=`zipinfo -t output.zip | head -n1 | cut -d " " -f1`
if [[ $exitstatus == 124 ]]; then
	numfiles3+=" TO"
fi
echo "$filename,$numfiles1,$numfiles2,$numfiles3" >> results.csv
echo "done with file: $filename"