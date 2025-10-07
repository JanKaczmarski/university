#include <signal.h>
#include <stdio.h>
#include <string.h>

#define TEST_SIGNAL SIGUSR1

void handle(int);

void handle(int signum) {
    printf("Obsluga: %d\n", signum);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Please provide positional argument!\n");
        printf("Possible options are: none | ignore | handler | mask\n");
        return 1;
    }

    char* option = argv[1];

    if (strcmp(option, "none") == 0) {
        printf("Passed none\n");
    } else if (strcmp(option, "ignore") == 0) {
        printf("Passed ignore\n");
        signal(TEST_SIGNAL, SIG_IGN);
    } else if (strcmp(option, "handler") == 0) {
        printf("Passed handler\n");
        signal(TEST_SIGNAL, handle);
    } else if (strcmp(option, "mask") == 0) {
        printf("Passed mask\n");
        sigset_t newmask, oldmask, set;

        sigemptyset(&newmask);
        sigaddset(&newmask, TEST_SIGNAL);

        printf("I will perform SIG_BLOCK on mask set\n");
        sigprocmask(SIG_BLOCK, &newmask, &oldmask);
    } else {
        printf("Wrong value passed: %s, please provide one of following:\n", option);
        printf("none | ignore | handler | mask\n");
        return 1;
    };


    raise(SIGUSR1);

    return 0;
}

