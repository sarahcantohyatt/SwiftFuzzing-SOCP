#!/bin/sh

# Saves outputs to a zipfile
flag=$1
./runFuzzer.sh $flag | java -jar zip_collate-assembly-1.0.jar -b 'file.' -e '.swift' -d '-----' output.zip
