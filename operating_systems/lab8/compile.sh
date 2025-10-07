#!/bin/bash

gcc -o bin/print src/print.c -pthread
gcc -o bin/client src/client.c -pthread
