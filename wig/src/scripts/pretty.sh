#!/bin/bash

COUNT=0
TOTAL=0
touch tmpempty
for i in ../examples/valid/*.wig
do
	if test -f "$i" 
	then
		./wigA -f $i -p >> tmpfile1 2> tmperror
		./wigA -f tmpfile1 -p >> tmpfile2 2> tmperror

		if diff tmpfile1 tmpfile2 >/dev/null && diff tmperror tmpempty >/dev/null; then
			COUNT=`expr $COUNT + 1`
		else
			echo "Printing error for " $i
			diff tmpfile1 tmpfile2
		fi
		rm tmpfile1 tmpfile2 tmperror
		TOTAL=`expr $TOTAL + 1`
	fi
done
rm tmpempty
echo "Pretty tests passed: " $COUNT "/" $TOTAL

