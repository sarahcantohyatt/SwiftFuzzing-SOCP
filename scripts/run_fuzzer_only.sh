#!/bin/sh

echo "filename,# of mutants lvl 1,# of mutants lvl 2,# of mutants lvl 3" > results.csv
./start.sh
./fuzzer_only.sh
for i in ../seedset/*.swift; do
	./next.sh
	./fuzzer_only.sh
done