#include <stdio.h>

#define STACK_SIZE  10

int stack[STACK_SIZE];
int top=0;   // jawne zerowanie nie jest konieczne

int stack_push(int x) {
    if (top==STACK_SIZE) return -2;
    else{
        stack[top++]=x;
        return 0.0;
    }
}

int stack_pop(void) {
    if (top==0) return -1;
    else return stack[--top];
}

int stack_state(void) {
    return top;
}

//Kolejka z przesuwaniem w tablicy

#define QUEUE_SIZE  10

int queue[QUEUE_SIZE];
int in=0, curr_nr=0;  // 1. klient dostanie nr = 1

int queue_push(int how_many) {
    if (how_many>QUEUE_SIZE-in){
        int temp=how_many;
        while (in<10){
            queue[in++]=++curr_nr;
            temp--;
        }
        curr_nr+=temp;
        return -2;
    }
    else{
        for (int i=0; i<how_many; i++) queue[in++]=++curr_nr;
        return 0;
    }
}

int queue_pop(int how_many) {
    if (how_many>in){
        in=0;
        return -1;
    }
    else{
        for (int i=0; i<in-how_many; i++) queue[i]=queue[i+how_many];
        in-=how_many;
        return in;
    }
}

int queue_state(void) {
    return in;
}

void queue_print(void) {
    for (int i=0; i<in; i++) printf("%d ",queue[i]);
}


//Kolejka w buforze cyklicznym

#define CBUFF_SIZE  10

int cbuff[CBUFF_SIZE];
int out=0, len=0;

int cbuff_push(int cli_nr) {
    if (len==CBUFF_SIZE) return -2;
    else{
        cbuff[(out+len++)%10]=cli_nr;
        return 0;
    }
}

int cbuff_pop(void) {
    if (len==0) return -1;
    else{
        len--;
        int temp=out;
        out=(out+1)%CBUFF_SIZE;
        return cbuff[temp];
    }
}

int cbuff_state(void) {
    return len;
}

void cbuff_print(void) {
    for (int i=0; i<len; i++) printf("%d ",cbuff[(out+i)%CBUFF_SIZE]);
}


int main(void) {
    int to_do, n, client_no, answer;
    scanf("%d", &to_do);
    switch(to_do) {
        case 1: // stack
            do {
                scanf("%d", &n);
                if (n > 0) {
                    if ((answer = stack_push(n)) < 0) printf("%d ", answer);
                } else if (n < 0) {
                    printf("%d ", stack_pop());
                } else printf("\n%d\n", stack_state());
            } while(n != 0);
            break;
        case 2: // FIFO queue with shifts
            do {
                scanf("%d", &n);
                if (n > 0) {
                    if ((answer = queue_push(n)) < 0) printf("%d ", answer);
                } else if (n < 0) {
                    if ((answer = queue_pop(-n)) < 0) printf("%d ", answer);
                } else {
                    printf("\n%d\n", queue_state());
                    queue_print();
                }
            } while(n != 0);
            break;
        case 3: // queue with cyclic buffer
            client_no = 0;
            do {
                scanf("%d", &n);
                if (n > 0) {
                    if ((answer = cbuff_push(++client_no)) < 0) printf("%d ", answer);
                } else if (n < 0) {
                    printf("%d ", cbuff_pop());
                } else {
                    printf("\n%d\n", cbuff_state());
                    cbuff_print();
                }
            } while(n != 0);
            break;
        default:
            printf("NOTHING TO DO!\n");
    }
    return 0;
}
