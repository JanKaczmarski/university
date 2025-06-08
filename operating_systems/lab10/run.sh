#!/bin/bash


gcc -pthread -o bin/main src/main.c -lm

./bin/main 20 3

rm -f bin/main
