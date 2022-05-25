#!/bin/bash

echo "Введите НЕЧЁТНОЕ число N равное стороне треугольника."

read value

result=$(expr $value  %  2)

if [[ $result -ne 0 ]]

then
        for (( i = $value; i > 0; i-- ))
        do
        for (( j = $value; j > 0; j-- ))
        do
        echo -e "* \c"
        done
        echo " "
        value=$(( $value - 1 ))
        done

else
        echo "Введено ЧЁТНОЕ число! Попробуйте еще раз."
fi
