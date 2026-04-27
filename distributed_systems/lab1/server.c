#include <signal.h>
#include <string.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <netinet/in.h>
#include <pthread.h>
#include <errno.h>

#define MAX_CLIENTS      64
#define BUF_SIZE         2048
#define THREAD_POOL_SIZE 4
#define QUEUE_SIZE       64

volatile sig_atomic_t keep_running = 1;

int g_server_socket = -1;
int g_udp_socket = -1;
int g_clients[MAX_CLIENTS];
int g_clientCount = 0;

pthread_mutex_t g_clients_mutex = PTHREAD_MUTEX_INITIALIZER;

// Thread pool
int g_queue[QUEUE_SIZE];
int g_queue_head  = 0;
int g_queue_tail  = 0;
int g_queue_count = 0;

pthread_mutex_t g_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  g_queue_cond  = PTHREAD_COND_INITIALIZER;

pthread_t g_thread_pool[THREAD_POOL_SIZE];
pthread_t g_udp_thread;


void cleanup() {
    if (g_server_socket != -1) { close(g_server_socket); g_server_socket = -1; }
    if (g_udp_socket    != -1) { close(g_udp_socket);    g_udp_socket    = -1; }

    pthread_mutex_lock(&g_clients_mutex);
    for (int i = 0; i < g_clientCount; i++) {
        if (g_clients[i] != -1) {
            shutdown(g_clients[i], SHUT_RDWR);
        }
    }
    pthread_mutex_unlock(&g_clients_mutex);

    // Wake up all worker threads to see that program finished
    pthread_cond_broadcast(&g_queue_cond);
    for (int i = 0; i < THREAD_POOL_SIZE; i++) {
        pthread_join(g_thread_pool[i], NULL);
    }

    pthread_mutex_destroy(&g_clients_mutex);
    pthread_mutex_destroy(&g_queue_mutex);
    pthread_cond_destroy(&g_queue_cond);
}

void sigint_handler(int sig) {
    (void)sig;
    keep_running = 0;
}


int add_client(int socket_fd) {
    pthread_mutex_lock(&g_clients_mutex);
    if (g_clientCount >= MAX_CLIENTS) {
        pthread_mutex_unlock(&g_clients_mutex);
        return -1;
    }
    g_clients[g_clientCount++] = socket_fd;
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

void broadcast(const char *msg, int sender_socket_fd) {
    pthread_mutex_lock(&g_clients_mutex);
    for (int i = 0; i < g_clientCount; i++) {
        if (g_clients[i] == sender_socket_fd) continue;
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
        int bytes_received = recvfrom(g_udp_socket, buffer, sizeof(buffer) - 1, 0,
                                      (struct sockaddr *)&sender_addr, &sender_len);
        if (bytes_received <= 0) {
            if (!keep_running) break;
            continue;
        }
        buffer[bytes_received] = '\0';
        printf("[SERVER UDP ROUTER]: Received a packet, broadcasting...\n");

        pthread_mutex_lock(&g_clients_mutex);
        for (int i = 0; i < g_clientCount; i++) {
            struct sockaddr_in client_addr;
            socklen_t client_len = sizeof(client_addr);

            if (getpeername(g_clients[i], (struct sockaddr *)&client_addr, &client_len) == -1) {
                printf("[SERVER UDP ERR]: failed to get peer name\n");
                continue;
            }
            if (client_addr.sin_port        == sender_addr.sin_port &&
                client_addr.sin_addr.s_addr == sender_addr.sin_addr.s_addr) {
                continue;
            }
            if (sendto(g_udp_socket, buffer, bytes_received, 0,
                       (struct sockaddr *)&client_addr, client_len) == -1) {
                perror("UDP broadcast sendto failed");
            }
        }
        pthread_mutex_unlock(&g_clients_mutex);
    }
    return NULL;
}

void *worker_thread(void *arg) {
    (void)arg;

    while (keep_running) {
        pthread_mutex_lock(&g_queue_mutex);

        while (g_queue_count == 0 && keep_running) {
            pthread_cond_wait(&g_queue_cond, &g_queue_mutex);
        }

        if (!keep_running) {
            pthread_mutex_unlock(&g_queue_mutex);
            break;
        }

        int client_sock = g_queue[g_queue_head];
        g_queue_head = (g_queue_head + 1) % QUEUE_SIZE;
        g_queue_count--;

        pthread_mutex_unlock(&g_queue_mutex);

        add_client(client_sock);

        // handle client for the connection duration
        char buffer[BUF_SIZE];
        while (keep_running) {
            int bytes_received = recv(client_sock, buffer, sizeof(buffer) - 1, 0);
            if (bytes_received <= 0) {
                printf("Client (fd: %d) disconnected.\n", client_sock);
                break;
            }
            buffer[bytes_received] = '\0';
            printf("[SERVER BROADCAST]: %s\n", buffer);
            broadcast(buffer, client_sock);
        }

        if (keep_running) {
            close(client_sock);
            remove_client(client_sock);
        }
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <server_port>\n", argv[0]);
        return 1;
    }

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
    signal(SIGPIPE, SIG_IGN);

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family      = AF_INET;
    server_addr.sin_port        = htons((uint16_t)port);
    server_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    // TCP socket
    g_server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (g_server_socket == -1) { perror("server socket"); return 1; }

    int opt = 1;
    setsockopt(g_server_socket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    if (bind(g_server_socket, (struct sockaddr *)&server_addr, sizeof(server_addr)) != 0) {
        perror("bind server socket"); cleanup(); return 1;
    }
    if (listen(g_server_socket, MAX_CLIENTS) != 0) {
        perror("listen"); cleanup(); return 1;
    }

    // UDP socket
    g_udp_socket = socket(AF_INET, SOCK_DGRAM, 0);
    if (g_udp_socket == -1) { perror("udp socket"); cleanup(); return 1; }

    int opt_udp = 1;
    setsockopt(g_udp_socket, SOL_SOCKET, SO_REUSEADDR, &opt_udp, sizeof(opt_udp));
    if (bind(g_udp_socket, (struct sockaddr *)&server_addr, sizeof(server_addr)) != 0) {
        perror("bind udp socket"); cleanup(); return 1;
    }

    // Uruchom wątek UDP
    if (pthread_create(&g_udp_thread, NULL, handle_udp_traffic, NULL) != 0) {
        perror("udp thread creation"); cleanup(); return 1;
    }
    pthread_detach(g_udp_thread);

    // Start thread pool once
    printf("Starting thread pool (%d workers)...\n", THREAD_POOL_SIZE);
    for (int i = 0; i < THREAD_POOL_SIZE; i++) {
        if (pthread_create(&g_thread_pool[i], NULL, worker_thread, NULL) != 0) {
            perror("thread pool creation"); cleanup(); return 1;
        }
    }

    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

    printf("Server started on port %ld. Waiting for connections...\n", port);
    printf("Thread pool: %d workers, queue capacity: %d\n", THREAD_POOL_SIZE, QUEUE_SIZE);

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

        pthread_mutex_lock(&g_queue_mutex);
        if (g_queue_count >= QUEUE_SIZE) {
            // Queue is full, reject client
            printf("Queue full. Rejecting client (fd: %d)\n", client_sock);
            pthread_mutex_unlock(&g_queue_mutex);
            remove_client(client_sock);
            close(client_sock);
            continue;
        }

        g_queue[g_queue_tail] = client_sock;
        g_queue_tail = (g_queue_tail + 1) % QUEUE_SIZE;
        g_queue_count++;
        pthread_cond_signal(&g_queue_cond); // wake up worker thread
        pthread_mutex_unlock(&g_queue_mutex);

        printf("New client queued (fd: %d), queue size: %d/%d\n",
               client_sock, g_queue_count, QUEUE_SIZE);
    }

    cleanup();
    printf("Server shutting down cleanly.\n");
    return 0;
}
