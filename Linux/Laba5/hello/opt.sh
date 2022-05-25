#!/bin/sh

filename=$1

for i in "-O0" "-Os" "-O1" "-O2" "-O3" "-O2 -march=native" "-O3 -march=native" "-O2 -march=native -funroll-loops" "-O3 -march=native -funroll-loops"
do
  echo "[ Build ] $i:"
  echo "....................."
  c++ -Wall -Wextra $i $filename -o prg.veg
  echo "  Time:"
  time ./prg.veg
  echo "....................."
  echo "  Disk Usage:"
  du -h $filename
  echo "_____________________"
done
