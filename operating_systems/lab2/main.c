#include <stdlib.h>
#include <stdio.h>
#include "collatz.h"

#define MAX_ITER 100

int collatz_conjecture(int input);
void test_collatz_convergence(int input, int max_iter, int *steps);

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("ERROR: Please provide `int input` as first program argument!\n");
        return 1;
    }

    int input = atoi(argv[1]);
    int i = 0;

    test_collatz_convergence(input, MAX_ITER, &i);

    return 0;
}

