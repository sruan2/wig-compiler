#!/bin/bash

COUNT=0
TOTAL=0
touch tmpempty
for i in ../examples/valid/*.wig
do
	if test -f "$i" 
	then
		./wigA -f $i -w 2> tmperror
		if diff tmpempty tmperror >/dev/null; then
			COUNT=`expr $COUNT + 1`
		else
			echo "Weeded" $i
                        while read line; do echo "$line"; done < tmperror
		fi
		TOTAL=`expr $TOTAL + 1`
	fi
	rm tmperror
done
for i in ../examples/invalid-weeder/*.wig
do
        if test -f "$i"
        then
                ./wigA -f $i -w 2> tmperror
                if diff tmpempty tmperror >/dev/null; then
                         echo "Weed succeeded on bad file " $i
                         while read line; do echo "$line"; done < tmperror
                else
                         COUNT=`expr $COUNT + 1`
                fi
                TOTAL=`expr $TOTAL + 1`
        fi
        rm tmperror
done
rm tmpempty
echo "Weed tests passed: " $COUNT "/" $TOTAL

