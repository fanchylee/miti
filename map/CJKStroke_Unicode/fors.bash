#! /bin/bash
String=$1
i=0
while (( i++ < ${#String} ))
do
   char=${String:$((i-1)):1}
   echo "$char" |./select -
done
