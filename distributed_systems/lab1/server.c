#include <signal.h> // SIGINT
#include <string.h>
#include <sys/socket.h> // sockets
#include <stdio.h> // printf
#include <stdlib.h> // atoi
#include <unistd.h> // close
#include <netinet/in.h> // INADDR_ANY
#include <pthread.h>

#define MAX_CLIENTS 10 // max number of clients in a chat
#define BUF_SIZE 2048

int g_server_socket;
int g_clients[MAX_CLIENTS]; // list of sockets
int g_clientCount = 0; // number of clients connected, == number of sockets

pthread_mutex_t g_clients_mutex = PTHREAD_MUTEX_INITIALIZER;

void cleanup() {
    for (int i = 0; i < g_clientCount; i++) {
        close(g_clients[i]);
    }
    close(g_server_socket);

    printf("Server shutting down.\n");
}

void sigint_handler(int sig) {
    (void)sig; // int sig to satisfy func signature

    cleanup();
    exit(0);
}


// adds client to clients array, func is idempotent
int add_client(int socket_fd) {
    pthread_mutex_lock(&g_clients_mutex);

    if (g_clientCount >= MAX_CLIENTS) {
        pthread_mutex_unlock(&g_clients_mutex);
        return -1;
    }

    g_clients[g_clientCount] = socket_fd;
    g_clientCount++;

    pthread_mutex_unlock(&g_clients_mutex);
    return 0;
}

// removes client from clients array, func is idempotent
void remove_client(int socket_fd) {
    pthread_mutex_lock(&g_clients_mutex);

    for (int i = 0; i < g_clientCount; i++) {
        if (g_clients[i] == socket_fd) {
            for (int j = i; j < g_clientCount - 1; j++) {
                g_clients[j] = g_clients[j + 1];
            }
            g_clientCount--;
            break;
        }
    }

    pthread_mutex_unlock(&g_clients_mutex);
}

// broadcast sends msg to all clients expect the sender
void broadcast(char* msg, int sender_socket_fd) {
    pthread_mutex_lock(&g_clients_mutex);

    for (int i = 0; i < g_clientCount; i++) {
        if (g_clients[i] == sender_socket_fd) {
            continue;
        }
        if (send(g_clients[i], msg, strlen(msg), 0) == -1) {
            perror("broadcast send failed");
        }
    }

    pthread_mutex_unlock(&g_clients_mutex);
}

void *handle_client(void* arg) {
    int client_sock = *(int *)arg;
    free(arg);

    char buffer[BUF_SIZE];

    while (1) {
        memset(buffer, 0, sizeof(buffer)); // clear buffor before read

        int bytes_received = recv(client_sock, buffer, sizeof(buffer) - 1, 0);
        if (bytes_received <= 0) {
            printf("Client (Socket fd: %d) disconnected.\n", client_sock);
            break;
        }

        buffer[bytes_received] = '\0';

        printf("[SERVER BROADCAST]: %s\n", buffer);

        broadcast(buffer, client_sock);
    }

    close(client_sock);
    remove_client(client_sock);

    return NULL;
}



int main(int argc, char *argv[]) {
    // accept port as param
    if (argc < 2) {
        printf("Usage: %s <server_port>\n", argv[0]);
        return 1;
    }

    if (signal(SIGINT, sigint_handler) == SIG_ERR) {
        perror("signal");
        return 1;
    }

    g_server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (g_server_socket == -1) {
        perror("server socket");
        return 1;
    }

    // Faster cleanup of socket, normally need to wait around 60s for system cleanup
    int opt = 1;
    setsockopt(g_server_socket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(atoi(argv[1])); // this mmight use incorrect pointer semantics!
    server_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(g_server_socket, (struct sockaddr *)&server_addr, sizeof(server_addr)) != 0) {
        perror("bind server socket");
        cleanup();
        return 1;
    }

    if (listen(g_server_socket, MAX_CLIENTS) != 0) {
        perror("listen");
        cleanup();
        return 1;
    }

    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

    while (1) {
        int client_sock = accept(g_server_socket, (struct sockaddr *)&client_addr, &client_len);
        if (client_sock == -1) {
            perror("accept failed");
            continue;
        }

        if (add_client(client_sock) == -1) {
            printf("Server full. Rejecting client (fd: %d)\n", client_sock);
            close(client_sock);
            continue;
        }

        printf("New client connected! Socket fd: %d\n", client_sock);

        int *new_sock = malloc(sizeof(int));
        *new_sock = client_sock;

        pthread_t thread_id;

        if (pthread_create(&thread_id, NULL, handle_client, (void *)new_sock) != 0) {
            perror("thread creation");
            free(new_sock);
            remove_client(client_sock);
            close(client_sock);
            continue;
        }

        pthread_detach(thread_id);
    }

    cleanup();
    return 0;
}
