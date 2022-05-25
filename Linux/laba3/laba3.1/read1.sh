#!/bin/bash

egrep -i '[a-z]*a[a-z]*' ./example.txt | tac > ./job1.txt
