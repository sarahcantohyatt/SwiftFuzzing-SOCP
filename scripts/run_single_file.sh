#!/bin/sh

swiftc -o $1.executable $1 > $1.output 2>&1 

if [ ! -f $1.executable ]
then
    touch $1.noexecutable
elif grep -q 'Stack dump' $1.output
then
    touch $1.crash
else
    rm $1 $1.executable $1.output
fi