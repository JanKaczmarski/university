# include <stdio.h>
# include <math.h>

int decreasing(int n){
    int last = n % 10;
    n /= 10;

    while(n > 0){
        if(n%10 > last){
            return 1;
        }
        last = n%10;
        n /= 10;
    }

    return 0;
}

int is_prime(int n){
    if(n==2 || n==3){
        return 0;
    } 
    else if(n%2==0 || n%3==0 || n==1){
        return 1;
    }

    int i = 6;
    while(i <= sqrt(n) + 1){
        if(n%(i-1) == 0 || n%(i+1) == 0){
            return 1;
        }
        i += 6;
    }
    
    return 0;
}

int main(void){
    int n = 0;
    printf("Give n: ");
    scanf("%d", &n);
    int cnt = 0;

    for(int i = 2; i<n; ++i){
        if(is_prime(i) == 0 && decreasing(i) == 0){
            printf("Number: %d\n", i);
            cnt += 1;
            }
    }
    printf("Total numbers: %d\n", cnt);
    return 0;
}
