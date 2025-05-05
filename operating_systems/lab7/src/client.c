#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mqueue.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#define SERVER_QUEUE "/server_queue"
#define MAX_MSG_SIZE 256

char client_queue_name[64];
mqd_t client_qd;

void handle_notification(int sig) {
    char buffer[MAX_MSG_SIZE];
    unsigned int prio;
    while (mq_receive(client_qd, buffer, MAX_MSG_SIZE, &prio) >= 0) {
        printf("Received: %s\n", buffer);
    }

    // Re-arm notification
    struct sigevent sev;
    sev.sigev_notify = SIGEV_SIGNAL;
    sev.sigev_signo = SIGUSR1;
    mq_notify(client_qd, &sev);
}

int main() {
    // 1. Tworzymy unikalną nazwę dla klienta
    pid_t pid = getpid();
    snprintf(client_queue_name, sizeof(client_queue_name), "/client_%d", pid);

    struct mq_attr attr = {
        .mq_flags = 0,
        .mq_maxmsg = 10,
        .mq_msgsize = MAX_MSG_SIZE,
        .mq_curmsgs = 0
    };

    client_qd = mq_open(client_queue_name, O_CREAT | O_RDONLY, 0666, &attr);
    if (client_qd == -1) {
        perror("mq_open client");
        exit(1);
    }

    // 2. Otwieramy kolejkę serwera
    mqd_t server_qd = mq_open(SERVER_QUEUE, O_WRONLY);
    if (server_qd == -1) {
        perror("mq_open server");
        exit(1);
    }

    // 3. Wysyłamy INIT do serwera
    char init_msg[MAX_MSG_SIZE];
    snprintf(init_msg, sizeof(init_msg), "INIT|%s", client_queue_name);
    mq_send(server_qd, init_msg, strlen(init_msg) + 1, 1);

    // 4. Tymczasowo otwieramy naszą kolejkę jako READ/WRITE by odebrać ID
    mqd_t client_rw_qd = mq_open(client_queue_name, O_RDWR);
    char buffer[MAX_MSG_SIZE];
    mq_receive(client_rw_qd, buffer, MAX_MSG_SIZE, NULL);
    int client_id = atoi(buffer);
    printf("Assigned client ID: %d\n", client_id);

    // 5. Rozdzielenie na procesy
    pid_t child = fork();
    if (child == 0) {
        // Dziecko: odbieranie wiadomości
        struct sigaction sa;
        sa.sa_handler = handle_notification;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = 0;
        sigaction(SIGUSR1, &sa, NULL);

        struct sigevent sev;
        sev.sigev_notify = SIGEV_SIGNAL;
        sev.sigev_signo = SIGUSR1;
        mq_notify(client_qd, &sev);

        while (1) pause();  // czekamy na SIGUSR1

    } else {
        char input[256];
        char buffer[512];

        while (fgets(input, sizeof(input), stdin) != NULL) {
            input[strcspn(input, "\n")] = 0;

            // Format: "<client_id>|<message>"
            snprintf(buffer, sizeof(buffer), "%d|%s", client_id, input);

            if (mq_send(server_qd, buffer, strlen(buffer) + 1, 0) == -1) {
                perror("mq_send");
            }
        }
    }

    // Posprzątaj (w praktyce raczej nigdy się nie wykona)
    mq_close(server_qd);
    mq_close(client_qd);
    mq_unlink(client_queue_name);
    return 0;
}
