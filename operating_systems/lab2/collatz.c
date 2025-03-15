#include "collatz.h"
#include <stdio.h>
#include <stdlib.h>

int collatz_conjecture(int input) {
    return (input % 2 == 0) ? input / 2 : 3 * input + 1;
}

void test_collatz_convergence(int input, int max_iter, int *steps) {
    int *output = (int *)malloc(max_iter * sizeof(int));
    if (!output) {
        printf("Memory allocation failed!\n");
        return;
    }
    
    int *ptr_out = output;
    
    *ptr_out = input;
    ptr_out++;

    for (; *steps < max_iter && input != 1; ++(*steps)) {
        input = collatz_conjecture(input);
        
        *ptr_out = input;
        ptr_out++;
    }

    // Check if the path exceeds max_iter
    if (*steps >= max_iter) {
        printf("Error: The path exceeds the maximum allowed iterations. Max Iterations: %d.\n", max_iter);
    } else {
        printf("Total steps taken: %d\n", *steps);
        printf("Collatz path: ");

        for (int i = 0; i <= *steps; ++i) {
            printf("%d", output[i]);
            if (i != *steps){
                printf("->");
            }
        }

        printf("\n");
    }

    free(output);
}
