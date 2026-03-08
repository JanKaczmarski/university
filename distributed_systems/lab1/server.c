#include <signal.h>
#include <string.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <netinet/in.h>
#include <pthread.h>
#include <errno.h>

#define MAX_CLIENTS 10
#define BUF_SIZE 2048

volatile sig_atomic_t keep_running = 1; // Async-signal-safe flag for graceful shutdown

int g_server_socket = -1; // Initialized to -1 to avoid closing stdin on early failure
int g_udp_socket = -1;
int g_clients[MAX_CLIENTS];
int g_clientCount = 0;

pthread_mutex_t g_clients_mutex = PTHREAD_MUTEX_INITIALIZER;

void cleanup() {
    pthread_mutex_lock(&g_clients_mutex);
    for (int i = 0; i < g_clientCount; i++) {
        if (g_clients[i] != -1) {
            close(g_clients[i]);
            g_clients[i] = -1;
        }
    }
    pthread_mutex_unlock(&g_clients_mutex);

    if (g_server_socket != -1) {
        close(g_server_socket);
        g_server_socket = -1;
    }
    if (g_udp_socket != -1) {
        close(g_udp_socket);
        g_udp_socket = -1;
    }

    pthread_mutex_destroy(&g_clients_mutex);
}

void sigint_handler(int sig) {
    (void)sig; 
    keep_running = 0; // Defer actual cleanup to the main loop
}

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

void broadcast(const char* msg, int sender_socket_fd) {
    pthread_mutex_lock(&g_clients_mutex);

    for (int i = 0; i < g_clientCount; i++) {
        if (g_clients[i] == sender_socket_fd) {
            continue;
        }
        
        // MSG_NOSIGNAL prevents SIGPIPE crash when a client disconnects
        // during broadcast — without this, the server would crash on broken pipe.
        if (send(g_clients[i], msg, strlen(msg), MSG_NOSIGNAL) == -1) {
            perror("broadcast send failed");
        }
    }

    pthread_mutex_unlock(&g_clients_mutex);
}

void *handle_udp_traffic(void *arg) {
    (void)arg;
    char buffer[BUF_SIZE];
    struct sockaddr_in sender_addr;
    socklen_t sender_len = sizeof(sender_addr);

    while (keep_running) {
        int bytes_received = recvfrom(g_udp_socket, buffer, sizeof(buffer) - 1, 0, (struct sockaddr*)&sender_addr, &sender_len);
        if (bytes_received <= 0) {
            if (!keep_running) break; // Exit cleanly if interrupted by shutdown
            continue;
        }

        buffer[bytes_received] = '\0';
        printf("[SERVER UDP ROUTER]: Received a packet, broadcasting...\n");

        pthread_mutex_lock(&g_clients_mutex);
        for (int i = 0; i < g_clientCount; i++) {
            struct sockaddr_in client_addr;
            socklen_t client_len = sizeof(client_addr);

            if (getpeername(g_clients[i], (struct sockaddr*)&client_addr, &client_len) == -1) {
                printf("[SERVER UDP ERR]: failed to get peer name\n");
                continue;
            }

            if (client_addr.sin_port == sender_addr.sin_port && client_addr.sin_addr.s_addr == sender_addr.sin_addr.s_addr) {
                continue;
            }

            if (sendto(g_udp_socket, buffer, bytes_received, 0, (struct sockaddr*)&client_addr, client_len) == -1) {
                perror("UDP broadcast sendto failed");
            }
        }
        pthread_mutex_unlock(&g_clients_mutex);
    }
    return NULL;
}

void *handle_client(void* arg) {
    int client_sock = *(int *)arg;
    free(arg);

    char buffer[BUF_SIZE];

    while (keep_running) {
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
    if (argc < 2) {
        printf("Usage: %s <server_port>\n", argv[0]);
        return 1;
    }

    // validate user input
    char *endptr;
    long port = strtol(argv[1], &endptr, 10);
    if (*endptr != '\0' || port < 1 || port > 65535) {
        fprintf(stderr, "Invalid port number: %s (must be 1-65535)\n", argv[1]);
        return 1;
    }

    if (signal(SIGINT, sigint_handler) == SIG_ERR) {
        perror("signal");
        return 1;
    }

    // Ignore SIGPIPE globally so broken client connections don't crash the server
    signal(SIGPIPE, SIG_IGN);

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr)); // Zero out padding to avoid garbage values
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons((uint16_t)port);
    server_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    g_server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (g_server_socket == -1) {
        perror("server socket");
        return 1;
    }
    
    int opt = 1; 
    setsockopt(g_server_socket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
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

    g_udp_socket = socket(AF_INET, SOCK_DGRAM, 0);
    if (g_udp_socket == -1) {
        perror("udp socket");
        cleanup();
        return 1;
    }

    int opt_udp = 1; 
    setsockopt(g_udp_socket, SOL_SOCKET, SO_REUSEADDR, &opt_udp, sizeof(opt_udp));
    if (bind(g_udp_socket, (struct sockaddr *)&server_addr, sizeof(server_addr)) != 0) {
        perror("bind udp socket");
        cleanup();
        return 1;
    }

    pthread_t udp_thread;
    if (pthread_create(&udp_thread, NULL, handle_udp_traffic, NULL) != 0) {
        perror("udp thread creation");
        cleanup();
        return 1;
    }
    pthread_detach(udp_thread);

    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

    printf("Server started on port %ld. Waiting for connections...\n", port);

    while (keep_running) {
        int client_sock = accept(g_server_socket, (struct sockaddr *)&client_addr, &client_len);
        if (client_sock == -1) {
            if (errno == EINTR) {
                if (!keep_running) break;
                continue;
            }
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
        if (new_sock == NULL) {
            perror("malloc failed");
            remove_client(client_sock);
            close(client_sock);
            continue;
        }
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
    printf("Server shutting down cleanly.\n");
    return 0;
}
