#!/bin/bash

NUMBER1=$(($1 + $2 - 1))
NUMBER2=$1
cat file.txt | head -n"$NUMBER1" | tail -n"$NUMBER2" > ./line.txt
