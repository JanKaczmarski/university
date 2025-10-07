#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Please provide number of processes to spawn, as first pos argument\n");
        return 1;
    }

    pid_t pid;
    int cap = atoi(argv[1]);
    int i;
    for(i = 0; i< cap; i++) {
        pid = fork();
        if (pid == 0) {
            printf("PPID: %d, PID: %d\n", getppid(), getpid());
            return 0;
        }
    }

    while(wait(0) > 0);

    printf("All finished!\n");
    return 0;
}
