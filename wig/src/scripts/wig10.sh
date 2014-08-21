#!/bin/bash

COUNT=0
TOTAL=0
touch tmpempty
for i in ../examples/valid/*.wig
do
	if test -f "$i" 
	then
		../bin/wig10 $i | grep "Compile succefully" >> tmpfile
		if diff tmpfile tmpempty >/dev/null; then
			echo "Wig10 didn't compile correctly" $i
		else
			COUNT=`expr $COUNT + 1`
		fi
		rm tmpfile
		TOTAL=`expr $TOTAL + 1`
	fi
done
rm tmpempty
rm ../examples/valid/*.c
rm ../examples/valid/*.install
echo "Wig10 Compiler tests passed: " $COUNT "/" $TOTAL

