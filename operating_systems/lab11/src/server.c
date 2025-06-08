
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <time.h>

#define MAX_CLIENTS 10
#define BUFFER_SIZE 1024
#define MAX_NAME 32

/**
 * Represents a connected client.
 *
 * Each client occupies a slot in the `clients` array.
 * The `active` field indicates whether the slot is currently used.
 */
typedef struct {
    int sockfd;              // Socket file descriptor used to communicate with client
    char name[MAX_NAME];     // Client's name, sent during initial connection
    struct sockaddr_in addr; // Address info of client
    time_t last_alive;       // Timestamp of the most recent ALIVE signal from client
    int active;              // 1 if slot is in use, 0 otherwise
} Client;

/**
 * Global array holding all connected clients.
 *
 * The array is protected by the `clients_mutex` and should be accessed only when locked.
 */
Client clients[MAX_CLIENTS];
/**
 * Mutex protecting access to the clients array.
 * All accesses (read or write) to the `clients` array must be guarded with this mutex.
 */
pthread_mutex_t clients_mutex = PTHREAD_MUTEX_INITIALIZER;
int server_sock;

/**
 * Sends a message to all connected clients, except for the one with the specified socket.
 *
 * @param message    The message to be broadcasted.
 * @param exclude_fd Socket file descriptor of the client to exclude (usually the sender).
 */
void broadcast(const char *message, int exclude_fd) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; ++i) {
        if (clients[i].active && clients[i].sockfd != exclude_fd) {
            write(clients[i].sockfd, message, strlen(message));
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

/**
 * Sends a private message to the specified client.
 *
 * @param to      Name of the client to receive the message.
 * @param message Message content to be sent.
 */
void private_message(const char *to, const char *message) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; ++i) {
        if (clients[i].active && strcmp(clients[i].name, to) == 0) {
            write(clients[i].sockfd, message, strlen(message));
            break;
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

/**
 * Sends a list of active client names to the requesting client.
 *
 * @param sockfd Socket of the requesting client to send response to.
 */
void list_clients(int sockfd) {
    pthread_mutex_lock(&clients_mutex);
    char list[BUFFER_SIZE] = "Active clients:\n";
    for (int i = 0; i < MAX_CLIENTS; ++i) {
        if (clients[i].active) {
            strcat(list, clients[i].name);
            strcat(list, "\n");
        }
    }
    pthread_mutex_unlock(&clients_mutex);
    write(sockfd, list, strlen(list));
}

/**
 * Removes a client from the list and closes their socket.
 *
 * @param index Index in the clients array.
 */
void remove_client(int index) {
    pthread_mutex_lock(&clients_mutex);
    clients[index].active = 0;
    close(clients[index].sockfd);
    pthread_mutex_unlock(&clients_mutex);
}

/**
 * Thread function handling communication with a single client.
 *
 * Handles commands such as:
 *   - STOP: disconnect client
 *   - ALIVE: update last_alive timestamp
 *   - LIST: return list of active clients
 *   - 2ALL: send message to all
 *   - 2ONE: send message to specific user
 *
 * @param arg Pointer to integer (index in clients array). Must be malloc'ed.
 */
void *client_handler(void *arg) {
    int index = *(int *)arg;
    free(arg);

    // Buffer for storing incoming messages from the client
    char buffer[BUFFER_SIZE];
    // number of bytes that was read from client socket
    int len;

    while ((len = read(clients[index].sockfd, buffer, BUFFER_SIZE - 1)) > 0) {
        buffer[len] = '\0';
        clients[index].last_alive = time(NULL);

        if (strncmp(buffer, "STOP", 4) == 0) {
            remove_client(index);
            return NULL;
        } else if (strncmp(buffer, "ALIVE", 5) == 0) {
            continue; // just update timestamp
        } else if (strncmp(buffer, "LIST", 4) == 0) {
            list_clients(clients[index].sockfd);
        } else if (strncmp(buffer, "2ALL", 4) == 0) {
            char msg[BUFFER_SIZE];
            time_t now = time(NULL);
            char *time_str = ctime(&now);
            time_str[strcspn(time_str, "\n")] = '\0';
            snprintf(msg, sizeof(msg), "[2ALL][%s][%s] %s", clients[index].name, time_str ,buffer + 5);
            broadcast(msg, clients[index].sockfd);
        } else if (strncmp(buffer, "2ONE", 4) == 0) {
            char to[MAX_NAME];
            char *msg_start = strchr(buffer + 5, ' ');
            if (msg_start) {
                *msg_start = '\0';
                strcpy(to, buffer + 5);
                msg_start++;

                char msg[BUFFER_SIZE];
                time_t now = time(NULL);
                char *time_str = ctime(&now);
                time_str[strcspn(time_str, "\n")] = '\0';
                snprintf(msg, sizeof(msg), "[2ONE][%s][%s] %s", clients[index].name, time_str, msg_start);
                private_message(to, msg);
            }
        }
    }

    remove_client(index);
    return NULL;
}

/**
 * Background thread that checks for inactive clients every 10 seconds.
 *
 * Disconnects clients who haven't sent an ALIVE signal within the last 15 seconds.
 */
void *alive_checker(void *arg) {
    while (1) {
        sleep(10);
        time_t now = time(NULL);
        pthread_mutex_lock(&clients_mutex);
        for (int i = 0; i < MAX_CLIENTS; ++i) {
            if (clients[i].active && (now - clients[i].last_alive > 15)) {
                printf("Client %s timed out.\n", clients[i].name);
                remove_client(i);
            }
        }
        pthread_mutex_unlock(&clients_mutex);
    }
    return NULL;
}

/**
 * Signal handler for SIGINT (Ctrl+C).
 *
 * Gracefully shuts down the server by closing the listening socket and exiting.
 */
void sigint_handler(int sig) {
    close(server_sock);
    printf("\nServer shutting down.\n");
    exit(0);
}

/**
 * Main function.
 *
 * 1. Parses the port from command line.
 * 2. Sets up the listening socket.
 * 3. Starts the alive_checker thread.
 * 4. Accepts incoming client connections and spawns client handler threads.
 */
int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <PORT>\n", argv[0]);
        return 1;
    }

    int port = atoi(argv[1]);
    server_sock = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in server_addr;

    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr.s_addr = INADDR_ANY;

    if (bind(server_sock, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1) {
        perror("bind");
    }
    listen(server_sock, MAX_CLIENTS);

    signal(SIGINT, sigint_handler);

    pthread_t checker_thread;
    pthread_create(&checker_thread, NULL, alive_checker, NULL);

    printf("Server listening on port %d...\n", port);

    while (1) {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);
        // wait for new connection on server_sock to be established
        int client_sock = accept(server_sock, (struct sockaddr *)&client_addr, &client_len);

        char name_buf[MAX_NAME];
        int name_len = read(client_sock, name_buf, MAX_NAME - 1);
        name_buf[name_len] = '\0';

        pthread_mutex_lock(&clients_mutex);

        // find first valid place to put new client to
        // note that clients array stores zero-values and not garbage values
        int index = -1;
        for (int i = 0; i < MAX_CLIENTS; ++i) {
            if (!clients[i].active) {
                index = i;
                clients[i].sockfd = client_sock;
                strcpy(clients[i].name, name_buf);
                clients[i].addr = client_addr;
                clients[i].last_alive = time(NULL);
                clients[i].active = 1;
                break;
            }
        }
        pthread_mutex_unlock(&clients_mutex);

        if (index != -1) {
            int *arg = malloc(sizeof(int));
            *arg = index;
            pthread_t tid;
            pthread_create(&tid, NULL, client_handler, arg);
        } else {
            char *msg = "Server full.\n";
            write(client_sock, msg, strlen(msg));
            close(client_sock);
        }
    }

    return 0;
}
