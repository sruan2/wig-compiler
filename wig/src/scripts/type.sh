#!/bin/bash

COUNT=0
TOTAL=0
touch tmpempty
for i in ../examples/valid/*.wig
do
	if test -f "$i"
	then
		./wigA -f $i -t 2> tmperror
		if diff tmpempty tmperror >/dev/null; then
			COUNT=`expr $COUNT + 1`
		else
			echo "Typecheck failed" $i
		fi
		TOTAL=`expr $TOTAL + 1`
	fi
	rm tmperror
done
for i in ../examples/invalid-typechecking/*.wig
do
        if test -f "$i"
        then
                ./wigA -f $i -t 2> tmperror
                if diff tmpempty tmperror >/dev/null; then
                   echo "Typecheck succeeded on bad file " $i
                else
                   COUNT=`expr $COUNT + 1`
                fi
                TOTAL=`expr $TOTAL + 1`
        fi
        rm tmperror
done
rm tmpempty
echo "Typechecker tests passed: " $COUNT "/" $TOTAL
