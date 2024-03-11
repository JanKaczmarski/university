#include <stdio.h>

int main(void) {

    int n = 0, a = 0, b=1, c = 0;
    printf("Enter n: ");
    scanf("%d", &n);

    while(n >= a*b){
        if(a*b == n){
            printf("Tak\n"); // there is no print in C lang
            return 0;
        }

        c = b;
        b = a + b;
        a = c;
    }

    printf("Nie\n");
    return 0;
}

