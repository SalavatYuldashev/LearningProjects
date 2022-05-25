#!/bin/bash

#калькулятор
#echo "$#"
task=""
step=" "
if [[ -n $1 ]]
then
  if [[ $[ $# % 2 ] -ne 0 ]]
  then
       count=1
        #echo $*
        while [[ $count -le $# ]]
        do
          if [[ $[ $count % 2 ] -ne 0 ]]
           then
            value=$( eval echo "\${${count}}" )
             if [[ $value =~ ^-?[0-9]+[.]?[0-9]*$ ]]
             then
                task="$task$step$value"
             else
                echo "Параметр '$( eval echo "\${${count}}")' не является числом!"
                exit 1
             fi
          else
            value=$( eval echo "\${${count}}" )
             case $value in
               "+")
                   task="$task$step$value"
                   ;;
               "-")
                   task="$task$step$value"
                   ;;
               "x"|"X")
                   task="$task$step$value"
                   ;;
               "/")
                    value=$( eval echo "\${${count}}" )
                    ncount=$[$count + 1]
                    nvalue=$( eval echo "\${${ncount}}" )
                    if [[ $nvalue != 0 && $nvalue != 0.0 ]]
                    then
                     task="$task$step$value"
                    else
                     echo "Деление на 0 запрещено!"
                     exit 1
                    fi
                    ;;
                *)
                  echo "Операнд '$( eval echo "\${${count}}")' не допустим!"
                        exit 1
                  ;;
             esac
           fi
        count=$[ $count + 1 ]
        done
        #res=$(echo "$task" | bc -l)
        res=$(echo "scale=6;$*" | sed -r 's/x|X/\*/g' | bc -l)
        echo "$task = $res"
      else
        echo "Введено неверное количество параметров!"
      fi
else
    echo "Вы не ввели пример!"
fi
#echo $task
