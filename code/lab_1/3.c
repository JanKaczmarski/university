#include <stdio.h>

int main(void)
{
    int n, sol = 0;
    printf("Provide n: ");
    scanf("%d", &n);
    for (int num = 6; num <= n; ++num)
    {
        int sum = 0;
        for (int i = 1; i < num; ++i){
            if(num % i == 0){
                sum += i;
            }
        }
        if(sum == num){
            printf("%d\n", num);
            sol += 1;
        }
    }

    printf("sol: %d\n", sol);
    return 0;
}
