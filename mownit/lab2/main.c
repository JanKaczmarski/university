#include <stdio.h>
#include <gsl/gsl_ieee_utils.h>

// Please use below line for compiling. It specifies directory with gsl installed
// gcc main.c -I/opt/homebrew/include -L/opt/homebrew/lib -lgsl -lgslcblas -Wall -o main

int main (void) {
    float n = 1.0;

    for (; n > 0; n = n / 16.5) {
        gsl_ieee_printf_float(&n);
        printf("\n");
    }

    gsl_ieee_printf_float(&n);
    printf("\n");

    return 0;
}
