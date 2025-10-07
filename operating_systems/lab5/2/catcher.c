
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>

// Atomic integers (safe for multithreading systems)
volatile sig_atomic_t received_signals = 0;
volatile sig_atomic_t mode = 0;
volatile sig_atomic_t running = 1;
pid_t sender_pid = -1;

void sigint_handler(int sig) {
    printf("Wciśnięto CTRL+C\n");
}

void handle_sigusr1(int sig, siginfo_t *info, void *ucontext) {
    sender_pid = info->si_pid;
    mode = info->si_value.sival_int;
    received_signals++;

    switch (mode) {
        case 1:
            printf("Liczba otrzymanych żądań zmiany trybu: %d\n", received_signals);
            break;

        case 2:
            while (mode == 2) {
                printf("Numer: %d\n", received_signals);
                sleep(1);
            }
            break;

        case 3:
            signal(SIGINT, SIG_IGN);
            break;

        case 4:
            signal(SIGINT, sigint_handler);
            break;

        case 5:
            running = 0;
            break;

        default:
            break;
    }

    kill(sender_pid, SIGUSR1);
}

int main() {
    printf("Catcher PID: %d\n", getpid());

    struct sigaction sa;
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = handle_sigusr1;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGUSR1, &sa, NULL);

    while (running) {
        pause();
    }

    return 0;
}

