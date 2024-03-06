#include <stdio.h>
#include <math.h>

double val(int tier, int a){
    if(tier == 0){
        return cos(a);        
    } 
    else if (tier == 1){
        return -sin(a);
    }
    else if (tier == 2){
        return -cos(a);
    }
    else{
        return sin(a);
    } 
    return 0;
}

int main(void){
    double x, y, step, taylor = 0;
    printf("Give x: ");
    scanf("%lf", &x);
    printf("Give y: ");
    scanf("%lf", &y);
    printf("Give step: ");
    scanf("%lf", &step);

    while(x < y){
        taylor = (1 + ((-1.0)/2.0*x*x) + 1.0/24.0*x*x);
        printf("x: %f, cos: %f, t_cos: %f\n", x, cos(x), taylor);
        x += step;
    }

    return 0;
}
