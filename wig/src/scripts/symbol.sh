#!/bin/bash

COUNT=0
TOTAL=0
touch tmpempty
for i in ../examples/valid/*.wig
do
	if test -f "$i"
	then
		./wigA -f $i -s true 2> tmperror >> tmptable
		if diff tmpempty tmperror >/dev/null; then
			COUNT=`expr $COUNT + 1`
		else
			echo "Symbol failed" $i
		fi
		TOTAL=`expr $TOTAL + 1`
	fi
	rm tmperror
done
for i in ../examples/invalid-symbol/*.wig
do
        if test -f "$i"
        then
                ./wigA $i symbol true 2> tmperror >> tmptable
                if diff tmpempty tmperror >/dev/null; then
                        echo "Symbol succeeded on bad file " $i
                else
                        COUNT=`expr $COUNT + 1`
                fi
                TOTAL=`expr $TOTAL + 1`
        fi
        rm tmperror
done
rm tmpempty
rm tmptable
echo "Symbol tests passed: " $COUNT "/" $TOTAL

