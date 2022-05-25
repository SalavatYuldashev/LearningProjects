#!/bin/bash

ls file1 file2 2> error.txt | cat -n
cat <error.txt | cat -n >error2.txt

