#!/bin/sh

cd ../testprograms
filename=`ls` #get the name of the file
echo "starting file: $filename ..."
cd ../scripts
exps=`./countExps.sh` #get the annotated exp count
echo "running: $filename on level 1"
timeout -s SIGINT 5 ./run_fuzzer_get_inputs.sh -1 #run fuzzer on lvl 1 for 5 seconds, we need the sigint flag so the os can clean up the zip file
numfiles1=`zipinfo -t output.zip | head -n1 | cut -d " " -f1` #get the num of mutants(only want number)
timeout 1m ./run_zipfile.sh #test the mutants on swiftc timing out after 1min
exitstatus=$?

#logic for creating directories abstracted to a function for readability
create_directories() {
	if [ -d ../triggers/$filename ]; then
		echo "" #nothing
	else
		mkdir ../triggers/$filename
	fi
	if [ -d ../triggers/$filename/$1 ]; then
		echo "" #nothing
	else
		mkdir ../triggers/$filename/$1
	fi
}

#logic for saving bug triggers abstracted to a function for re-use
save_files() {
	create_directories $1
	failedOutput=`ls *.swift.output`
	for file in $failedOutput
	do
		mv $file ../triggers/$filename/$1
	done
	mutants=`ls *.swift`
	for file in $mutants
	do
		mv $file ../triggers/$filename/$1
	done
	countfile=`ls *.swift.noexecutable 2>/dev/null | wc -l`
	if [ $countfile != 0 ]; then
	#if [ -e ./*.swift.noexecutable ]; then
		failedFiles=`ls *.swift.noexecutable`
		for file in $failedFiles
		do
			mv $file ../triggers/$filename/$1
		done
	fi
	countfile=`ls *.swift.crash 2>/dev/null | wc -l`
	if [ $countfile != 0 ]; then
	#if [ -e ./*.swift.crash ]; then
		crashFiles=`ls *.swift.crash`
		for file in $crashFiles
		do
			mv $file ../triggers/$filename/$1
		done
	fi
}

#helper method because bash is bash
save_to_global() {
	if [ $2 == '1' ]; then
		numtested1=$1
	elif [ $2 == '2' ]; then
		numtested2=$1
	else
		numtested3=$1
	fi
}

#logic for checking for timeout and taking appropriate action abstracted to a function for re-use
check_timeout() {
	if [[ $exitstatus == 124 ]]; then
		#we timed out
		countfile1=`ls *.swift.noexecutable 2>/dev/null | wc -l`
		countfile2=`ls *.swift.crash 2>/dev/null | wc -l`
		lastfile=`ls -1q *.swift.output | sort -V | tail -n 1`
		substring=$(echo $lastfile| cut -d'.' -f 2)
		save_to_global $substring $1
		if [ $countfile1 != 0 ] || [ $countfile2 != 0 ]; then
			#there are mutants that failed to compile or crashed, we will save them
			save_files $1
		fi
	elif [[ $exitstatus  == 0 ]]; then
		#we did not time out, we need to determine if there are any bug triggers
		countfile1=`ls *.swift.noexecutable 2>/dev/null | wc -l`
		countfile2=`ls *.swift.crash 2>/dev/null | wc -l`
		if [ $countfile1 != 0 ] || [ $countfile2 != 0 ]; then
			#there are mutants that failed to compile or crashed, we will save them
			save_files $1
		fi
	else
		echo "unknown exit"
	fi
}

cleanup() {
	sleep 2
	echo "cleaning up any extra files..."
	rm *.swift
	rm *.swift.output
	rm *.swift.crash
	rm *.swift.executable
	rm *.swift.noexecutable
}

check_timeout '1'
cleanup
#now lvl2
echo "running: $filename on level 2"
timeout -s SIGINT 10 ./run_fuzzer_get_inputs.sh -2
numfiles2=`zipinfo -t output.zip | head -n1 | cut -d " " -f1`
timeout 1m ./run_zipfile.sh
exitstatus=$?
check_timeout '2'
cleanup
#now lvl3
echo "running: $filename on level 3"
timeout -s SIGINT 15 ./run_fuzzer_get_inputs.sh -3
numfiles3=`zipinfo -t output.zip | head -n1 | cut -d " " -f1`
timeout 1m ./run_zipfile.sh
exitstatus=$?
check_timeout '3'
cleanup

echo "$filename,$exps,$numfiles1,$numtested1,$numfiles2,$numtested2,$numfiles3,$numtested3" >> results.csv
echo "done with file: $filename \n"