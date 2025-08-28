cd ..
cd seedset
filenames=`ls *.swift`
cd ..
for file in $filenames
do
	mv seedset/$file testprograms/$file
	break
done