#include <stdio.h>
#include <stdlib.h>

#define CBUFF_SIZE  52

void cbuff_push(int tab [], int cli_nr, char player);
int cbuff_pop(int tab [], char player);
int rand_from_interval(int a, int b);
void rand_permutation(int n, int tab []);
void print_deck(int tab [], char player);

int outa=0, outb=0, lena=CBUFF_SIZE/2, lenb=CBUFF_SIZE/2;

int main()
{
    int A[CBUFF_SIZE];
    int B[CBUFF_SIZE];
    int deck[CBUFF_SIZE];
    int version, seed, max_conflicts, curr_conflicts=0;
    scanf("%d %d %d",&seed, &version, &max_conflicts);
    srand(seed);
    rand_permutation(CBUFF_SIZE,deck);
    for (int i=0; i<CBUFF_SIZE/2; i++){
        A[i]=deck[i];
        B[i]=deck[i+CBUFF_SIZE/2];
    }
    switch (version){
    case 0:
    while (curr_conflicts<=max_conflicts){
        if (lena==0){
            printf("3\n");
            print_deck(B, 'b');
            break;
        }
        else if (lenb==0){
            printf("2 %d", curr_conflicts);
            break;
        }
        if ((int)A[outa]/4>(int)B[outb]/4){
            int a=cbuff_pop(A, 'a');
            int b=cbuff_pop(B, 'b');
            cbuff_push(A, a,'a');
            cbuff_push(A, b,'a');
        }
        else if ((int)A[outa]/4<(int)B[outb]/4){
            int a=cbuff_pop(A, 'a');
            int b=cbuff_pop(B, 'b');
            cbuff_push(B, b,'b');
            cbuff_push(B, a,'b');
        }
        else{
            int cards=3;
            curr_conflicts++;
            while (lena>=cards && lenb>=cards && (int)A[(outa+cards-1)%CBUFF_SIZE]/4==(int)B[(outb+cards-1)%CBUFF_SIZE]/4){
                cards+=2;
                curr_conflicts++;
                if (curr_conflicts>max_conflicts){
                    printf("0 %d %d", lena, lenb);
                    break;
                }
            }
            if (lena<cards || lenb<cards){
                printf("1 %d %d", lena, lenb);
                break;
            }
            else if ((int)A[(outa+cards-1)%CBUFF_SIZE]/4>(int)B[(outb+cards-1)%CBUFF_SIZE]/4){
                for (int i=0; i<cards; i++){
                    int a=cbuff_pop(A, 'a');
                    cbuff_push(A, a, 'a');
                }
                for (int i=0; i<cards; i++){
                    int b=cbuff_pop(B, 'b');
                    cbuff_push(A, b, 'a');
                }
            }
            else {
                for (int i=0; i<cards; i++){
                    int b=cbuff_pop(B, 'b');
                    cbuff_push(B, b, 'b');
                }
                for (int i=0; i<cards; i++){
                    int a=cbuff_pop(A, 'a');
                    cbuff_push(B, a, 'b');
                }
            }
        }
        curr_conflicts++;
    }
    if (curr_conflicts>max_conflicts){
        printf("0 %d %d", lena, lenb);
        break;
    }
    break;
    case 1:
    while (curr_conflicts<=max_conflicts){
        if (lena==0){
            printf("3\n");
            print_deck(B, 'b');
            break;
        }
        else if (lenb==0){
            printf("2 %d", curr_conflicts);
            break;
        }
        if ((int)A[outa]/4>(int)B[outb]/4){
            int a=cbuff_pop(A, 'a');
            int b=cbuff_pop(B, 'b');
            cbuff_push(A, a,'a');
            cbuff_push(A, b,'a');
        }
        else if ((int)A[outa]/4<(int)B[outb]/4){
            int a=cbuff_pop(A, 'a');
            int b=cbuff_pop(B, 'b');
            cbuff_push(B, b,'b');
            cbuff_push(B, a,'b');
        }
        else{
            int a=cbuff_pop(A, 'a');
            int b=cbuff_pop(B, 'b');
            cbuff_push(A, a,'a');
            cbuff_push(B, b,'b');
        }
        curr_conflicts++;
    }
    if (curr_conflicts>max_conflicts){
        printf("0 %d %d", lena, lenb);
        break;
    }
    break;
    default:
    break;
    }
    return 0;
}

void cbuff_push(int tab [], int cli_nr, char player) {
    if (player=='a') tab[(outa+lena++)%CBUFF_SIZE]=cli_nr;
    else tab[(outb+lenb++)%CBUFF_SIZE]=cli_nr;
}

int cbuff_pop(int tab [], char player) {
    if (player=='a'){
        if (lena==0) return -1;
        lena--;
        int temp=outa;
        outa=(outa+1)%CBUFF_SIZE;
        return tab[temp];
    }
    else{
        if (lenb==0) return -1;
        lenb--;
        int temp=outb;
        outb=(outb+1)%CBUFF_SIZE;
        return tab[temp];
    }
}

int rand_from_interval(int a, int b) {
    return rand()%(b-a+1)+a;
}

void rand_permutation(int n, int tab []) {
    int k, temp;
    for (int i=0; i<n; i++) tab[i]=i;
    for (int i=0; i<n-1; i++){
        k=rand_from_interval(i,n-1);
        temp=tab[i];
        tab[i]=tab[k];
        tab[k]=temp;
    }
}

void print_deck(int tab [], char player) {
    if (player=='b') {
        for (int i=0; i<lenb; i++) printf("%d ", tab[(outb+i)%CBUFF_SIZE]);
    }
    else{
        for (int i=0; i<lena; i++) printf("%d ", tab[(outa+i)%CBUFF_SIZE]);
    }
}
