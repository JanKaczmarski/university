#include <stdio.h>

int main(void){
    char sentence[20];
    //char c_sentence[20];

    printf("Give your sentence to be encoded: ");
    scanf("%s", sentence);
    printf("You entered: %s. \n", sentence);

    for(int i = 0; i < 20; ++i){
        printf("%s\n", &sentence);        
    }

    return 0;

}
