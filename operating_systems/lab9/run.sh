#!/bin/bash

if [ -z "$1" ]; then
    echo "INFO: Variable was not provided as first positional arg"
    echo "Using default 0.000000001"
    PREC=0.000000001
else
    PREC=$1
fi

gcc -pthread -o bin/main src/main.c -lm

for i in {1..5}; do
    echo "****Number of threads: $i****"
    time ./bin/main $PREC $i
done

rm -f bin/main
