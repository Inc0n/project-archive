#!/bin/bash

ls | while read i; do
    if [[ "$i" == *".h"* ]]; then
        echo $i
        while read line; do
            local_include="#include \""
            case $line in "$local_include"*)
                              # Your code here
                              # echo $i
                              include=${line/"$local_include"/}
                              include=${include::-1}
                              echo " $include"
            esac
            # echo $line
        done< <(cat "$i" | head -n 25)
    fi
done
