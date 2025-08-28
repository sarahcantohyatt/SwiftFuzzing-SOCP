#!/bin/sh
cd ..
mkdir seedset
cd swift-programs
filenames=`ls *.swift`
cd ..
for file in $filenames
do
	cp swift-programs/$file seedset/$file
done
mkdir testprograms
mkdir triggers
cd scripts

echo "filename,# of annotated exps,# of mutants lvl 1,# of programs tested if timeout lvl 1,# of mutants lvl 2,# of programs tested if timeout lvl2,# of mutants lvl 3,# of programs tested if timeout lvl3" > results.csv
./start.sh
./fully_automated.sh
for i in ../seedset/*.swift; do
	./next.sh
	./fully_automated.sh
done

cd ../testprograms
echo "Cleaning up any last files..."
rm *.swift
echo "Fuzzing campaign finished! Check SwiftFuzzing-SOCP/triggers for any bug triggers."