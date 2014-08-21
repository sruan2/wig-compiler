#!/bin/bash

COUNT=0
TOTAL=0
touch tmpempty
for i in ../examples/valid/*.wig
do
	if test -f "$i" 
	then
		../bin/pwig $i 2> tmpfile >> tmpfile
		if diff tmpfile tmpempty >/dev/null; then
			COUNT=`expr $COUNT + 1`
		else
			echo "Pwig didn't compile correctly" $i
		fi
		rm tmpfile
		TOTAL=`expr $TOTAL + 1`
	fi
done
rm tmpempty
rm ../examples/valid/*.c
rm ../examples/valid/*.install
rm ../examples/valid/*.h
echo "Pwig successful code generation: " $COUNT "/" $TOTAL

