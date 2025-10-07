
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mqueue.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>

#define SERVER_QUEUE_NAME "/print_queue"
#define MAX_MSG_SIZE 256
#define JOB_LENGTH 10

// Generate a random lowercase string of length JOB_LENGTH
void generate_random_job(char *buffer) {
    for (int i = 0; i < JOB_LENGTH; i++) {
        buffer[i] = 'a' + rand() % 26;
    }
    buffer[JOB_LENGTH] = '\0';  // null-terminate
}

int main() {
    srand(time(NULL) ^ getpid());  // Seed randomness per process

    mqd_t qd = mq_open(SERVER_QUEUE_NAME, O_WRONLY);
    if (qd == -1) {
        perror("mq_open");
        exit(1);
    }

    char buffer[MAX_MSG_SIZE];

    while (1) {
        generate_random_job(buffer);

        if (mq_send(qd, buffer, strlen(buffer) + 1, 0) == -1) {
            perror("mq_send");
        } else {
            printf("CLIENT: Sent job \"%s\"\n", buffer);
        }

        sleep(rand() % 5 + 1);  // Wait 1–5 seconds
    }

    mq_close(qd);
    return 0;
}
