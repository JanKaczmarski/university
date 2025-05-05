
#define _GNU_SOURCE
#include <mqueue.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CLIENTS 10
#define MAX_MSG_SIZE 256
#define SERVER_QUEUE "/server_queue"

typedef struct {
    int id;
    char queue_name[64];
    mqd_t queue;
} client_t;

client_t clients[MAX_CLIENTS];
int client_count = 0;

int main(void) {
    struct mq_attr attr = {
        .mq_flags = 0,
        .mq_maxmsg = 10,
        .mq_msgsize = MAX_MSG_SIZE,
        .mq_curmsgs = 0
    };

    mq_unlink(SERVER_QUEUE);
    mqd_t server_q = mq_open(SERVER_QUEUE, O_CREAT | O_RDONLY, 0666, &attr);
    if (server_q == -1) {
        perror("mq_open (server)");
        return 1;
    }

    char buffer[MAX_MSG_SIZE];
    unsigned int prio;

    while (1) {
        ssize_t bytes = mq_receive(server_q, buffer, MAX_MSG_SIZE, &prio);
        if (bytes == -1) {
            perror("mq_receive");
            continue;
        }

        buffer[bytes] = '\0';  // make sure it's null-terminated

        // INIT|/clientQueue
        if (strncmp(buffer, "INIT|", 5) == 0) {
            if (client_count >= MAX_CLIENTS) {
                printf("Too many clients\n");
                continue;
            }

            // Skip INIT| part of string, and go to queue name
            char *client_queue_name = buffer + 5;

            mqd_t client_q = mq_open(client_queue_name, O_WRONLY);
            if (client_q == -1) {
                perror("mq_open (client)");
                continue;
            }

            client_t new_client = {
                .id = client_count,
                .queue = client_q
            };
            strncpy(new_client.queue_name, client_queue_name, sizeof(new_client.queue_name));
            clients[client_count++] = new_client;

            char reply[16];
            snprintf(reply, sizeof(reply), "%d", new_client.id);
            mq_send(client_q, reply, strlen(reply) + 1, 1);

            printf("New client connected: %d (%s)\n", new_client.id, new_client.queue_name);
        } else {
            char *client_id = strtok(buffer, "|");
            char *msg = strtok(NULL, "");

            if (!client_id || !msg) {
                fprintf(stderr, "Malformed message\n");
                continue;
            }


            int sender_id = atoi(client_id);
            char full_msg[MAX_MSG_SIZE];
            snprintf(full_msg, sizeof(full_msg), "Client %d says: %s", sender_id, msg);

            // Broadcast to others
            for (int i = 0; i < client_count; ++i) {
                if (clients[i].id != sender_id) {
                    mq_send(clients[i].queue, full_msg, strlen(full_msg) + 1, 1);
                }
            }

            printf("Broadcasted message from client %d\n", sender_id);
        }
    }

    mq_close(server_q);
    mq_unlink(SERVER_QUEUE);
    return 0;
}

