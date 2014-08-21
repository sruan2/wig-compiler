#!/bin/bash

COUNT=0
TOTAL=0
touch tmpempty
for i in ../examples/invalid-syntax/*.wig
do
	if test -f "$i" 
	then
		./wigA -f $i -p >> tmpfile 2> tmperror

		if diff tmperror tmpempty >/dev/null; then
			echo "File with bad syntax passed" $i
		else
			COUNT=`expr $COUNT + 1`
		fi
		rm tmperror tmpfile
		TOTAL=`expr $TOTAL + 1`
	fi
done
rm tmpempty
echo "Syntax tests passed: " $COUNT "/" $TOTAL

