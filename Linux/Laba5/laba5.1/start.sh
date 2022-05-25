#!/bin/bash

filename=$1

for i in "-O0" "-Os" "-O1" "-O2" "-O3" "-O2 -march=native" "-O3 -march=native" \
"-O2 -march=native -funroll-loops" "-O3 -march=native -funroll-loops" \
"-O3 -march=native -funroll-loops -fipa-cp -flto" "-O3 -march=native -funroll-loops -fprofile-generate" \
"-O3 -march=native -funroll-loops -fipa-cp -flto -fprofile-generate"
do
  echo "_____________________"
  echo "  Optimization: $i:"
  echo "....................."
  c++ -Wall -Wextra $i $filename -o prg.veg
  echo "  Time:"
  time ./prg.veg 150 20
  echo "....................."
  echo "  Disk usage:"
  du -b $filename
  echo "_____________________"
done
