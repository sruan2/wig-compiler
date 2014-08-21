#!/bin/bash

COUNT=0
TOTAL=0
touch tmpempty
for i in ../examples/valid/*.wig
do
	if test -f "$i" 
	then
		./wigA -f $i -c >> tmpfile 2> tmperror

		if diff tmperror tmpempty >/dev/null; then
			COUNT=`expr $COUNT + 1`
		else
			echo "Couldn't generate code for" $i
		fi
		rm tmperror tmpfile
		TOTAL=`expr $TOTAL + 1`
	fi
done
for i in ../examples/invalid-codegen/*.wig
do
	if test -f "$i" 
	then
		./wigA -f $i -c >> tmpfile 2> tmperror

		if diff tmperror tmpempty >/dev/null; then
			echo "Invalid file got code-generated" $i
		else
			COUNT=`expr $COUNT + 1`
		fi
		rm tmperror tmpfile
		TOTAL=`expr $TOTAL + 1`
	fi
done
rm tmpempty
echo "Code generation tests passed: " $COUNT "/" $TOTAL

