#include <stdio.h>

int main(void) {

    typedef unsigned long long int byte;

    byte fact = 1;
	
    for(int n = 0; n <= 40; ++n){
        if (n == 0) {
            printf("1\n");
            continue;
        } 

        for(int i = 1; i <= n; ++i) {
            fact *= i;
        }
        printf("%d! = %llu\n", n, fact);
        fact = 1;
    }

	return 0;
}
