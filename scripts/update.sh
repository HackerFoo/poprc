#!/bin/bash

if [ -e $1 ] && cmp --silent $1 $2
then
    rm $2
else
    mv $2 $1
fi
