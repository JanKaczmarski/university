#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(void){
    srand(time(NULL)); // use time in seconds to get seed
    int x = rand() % 101;
    int m=50, l=0, r=100, cnt=0;
    while(l < m){
        if(x < m){
            r = m;
            m = (l + r)/2;
        }
        else if(x > m){
            l = m;
            m = (l + r)/2;
        }
        else {
            printf("Found x=%d, after %d moves.\n", x, cnt);
            return 0;
        }
        cnt += 1;
    }
    printf("Found x=%d, after %d moves.\n", x, cnt);

    return 0;
    }
