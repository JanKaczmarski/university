#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>

volatile sig_atomic_t confirmed = 0;

void handle_confirmation(int sig) {
    confirmed = 1;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Użycie: %s <PID_catchera> <tryb (1-5)>\n", argv[0]);
        return 1;
    }

    pid_t catcher_pid = (pid_t)atoi(argv[1]);
    int mode = atoi(argv[2]);

    struct sigaction sa;
    sa.sa_handler = handle_confirmation;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGUSR1, &sa, NULL);

    union sigval value;
    value.sival_int = mode;

    sigset_t mask, oldmask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGUSR1);
    sigprocmask(SIG_BLOCK, &mask, &oldmask);

    if (sigqueue(catcher_pid, SIGUSR1, value) == -1) {
        fprintf(stderr, "Failed to send signal\n");
        return 1;
    }

    while (!confirmed) {
        sigsuspend(&oldmask);
    }

    printf("Catcher message received!\n");
    return 0;
}
