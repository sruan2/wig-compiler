#!/bin/bash

touch tmpfile
echo "Which WiG program do you want to try?<br>" >> tmpfile
for i in ~/public_html/cgi-bin/*.cgi
do
	if test -f $i; then
		echo "<a href='${i##*/}'>${i##*/}</a><br>"  >> tmpfile
	fi
done
mv tmpfile ~/public_html/cgi-bin/index.html
chmod 755 ~/public_html/cgi-bin/index.html
