#!/bin/bash

FILE=$1

if [ -f "$FILE" ]; then
        echo "$FILE существует"
else
        echo "$FILE не существует"
fi
