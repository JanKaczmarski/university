#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

/* generate a random floating point number from min to max */
double randfrom(double min, double max) 
{
    double range = (max - min); 
    double div = RAND_MAX / range;
    return min + (rand() / div);
}

int main(void){
    // specify seed based on seconds
    srand(time(NULL));

    double a, b, N, cnt=0;
    
    printf("Give a: ");
    scanf("%lf", &a);

    printf("Give b: ");
    scanf("%lf", &b);

    printf("Give N: ");
    scanf("%le", &N);

    for(double i = 0; i < N; ++i){
        double x = randfrom(a, b), y = randfrom(0 ,1.0);
        if(y < sin(x)){
            cnt += 1;
        }
    }
    printf("Answer is %f\n", (cnt/N)*M_PI);

    return 0;
}
