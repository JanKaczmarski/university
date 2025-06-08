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
 * Server socket used to communicate with clients. The connections uses UDP protocol
 * for communication. So both sends and recv are done using this socket.
 */
int sock;

/**
 * Represents a connected client.
 *
 * Each client occupies a slot in the `clients` array.
 * The `active` field indicates whether the slot is currently used.
 */
typedef struct
{
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

/**
 * Sends a message to all connected clients, except for the one with the specified socket.
 *
 * @param message    The message to be broadcasted.
 * @param exclude_fd Socket file descriptor of the client to exclude (usually the sender).
 */
void broadcast(const char *message, int index)
{
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; ++i)
    {
        if (clients[i].active && i != index)
        {
            sendto(sock, message, strlen(message), 0, (struct sockaddr *)&clients[i].addr, sizeof(clients[i].addr));
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
void private_message(const char *to, const char *message)
{
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; ++i)
    {
        if (clients[i].active && strcmp(clients[i].name, to) == 0)
        {
            sendto(sock, message, strlen(message), 0, (struct sockaddr *)&clients[i].addr, sizeof(clients[i].addr));
            break;
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

/**
 * Sends a list of active client names to the requesting client.
 *
 * @param index of the requesting client to send response to.
 */
void list_clients(int index)
{
    pthread_mutex_lock(&clients_mutex);
    char list[BUFFER_SIZE] = "Active clients:\n";
    for (int i = 0; i < MAX_CLIENTS; ++i)
    {
        if (clients[i].active)
        {
            strcat(list, clients[i].name);
            strcat(list, "\n");
        }
    }
    pthread_mutex_unlock(&clients_mutex);
    sendto(sock, list, strlen(list), 0, (struct sockaddr *)&clients[index].addr, sizeof(clients[index].addr));
}

/**
 * Removes a client from the list.
 *
 * @param index Index in the clients array.
 */
void remove_client(int index)
{
    pthread_mutex_lock(&clients_mutex);
    clients[index].active = 0;
    pthread_mutex_unlock(&clients_mutex);
}

/**
 * Background thread that checks for inactive clients every 10 seconds.
 *
 * Disconnects clients who haven't sent an ALIVE signal within the last 15 seconds.
 */
void *alive_checker(void *arg)
{
    while (1)
    {
        sleep(10);
        time_t now = time(NULL);
        pthread_mutex_lock(&clients_mutex);
        for (int i = 0; i < MAX_CLIENTS; ++i)
        {
            if (clients[i].active && (now - clients[i].last_alive > 15))
            {
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
void sigint_handler(int sig)
{
    close(sock);
    printf("\nServer shutting down.\n");
    exit(0);
}

/**
 * Handles a message from a registered client
 */
void handle_client_message(int index, const char *buffer) {
    clients[index].last_alive = time(NULL);
    if (strncmp(buffer, "STOP", 4) == 0) {
        remove_client(index);
        printf("Client %s disconnected.\n", clients[index].name);
    } else if (strncmp(buffer, "ALIVE", 5) == 0) {
        // just update timestamp, already done
    } else if (strncmp(buffer, "LIST", 4) == 0) {
        list_clients(index);
    } else if (strncmp(buffer, "2ALL", 4) == 0) {
        char msg[BUFFER_SIZE];
        time_t now = time(NULL);
        char *time_str = ctime(&now);
        time_str[strcspn(time_str, "\n")] = '\0';
        snprintf(msg, sizeof(msg), "[2ALL][%s][%s] %s", clients[index].name, time_str, buffer + 5);
        broadcast(msg, index);
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

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <PORT>\n", argv[0]);
        return 1;
    }

    sock = socket(AF_INET, SOCK_DGRAM, 0);

    int port = atoi(argv[1]);
    struct sockaddr_in server_addr;

    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr.s_addr = INADDR_ANY;

    if (bind(sock, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1)
    {
        perror("bind");
        return 1;
    }
    signal(SIGINT, sigint_handler);

    pthread_t checker_thread;
    pthread_create(&checker_thread, NULL, alive_checker, NULL);

    printf("Server listening on port %d...\n", port);

    while (1)
    {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);
        char buffer[BUFFER_SIZE];
        int len = recvfrom(sock, buffer, BUFFER_SIZE - 1, 0, (struct sockaddr *)&client_addr, &client_len);
        if (len == -1)
        {
            perror("recvfrom");
            continue;
        }
        buffer[len] = '\0';

        // Check if a client with the same address is already active
        int index = -1;
        pthread_mutex_lock(&clients_mutex);
        for (int i = 0; i < MAX_CLIENTS; ++i)
        {
            if (clients[i].active &&
                clients[i].addr.sin_addr.s_addr == client_addr.sin_addr.s_addr &&
                clients[i].addr.sin_port == client_addr.sin_port)
            {
                index = i;
                break;
            }
        }
        pthread_mutex_unlock(&clients_mutex);

        if (index == -1) {
            // New client registration
            pthread_mutex_lock(&clients_mutex);
            for (int i = 0; i < MAX_CLIENTS; ++i)
            {
                if (!clients[i].active)
                {
                    index = i;
                    strncpy(clients[i].name, buffer, MAX_NAME - 1);
                    clients[i].name[MAX_NAME - 1] = '\0';
                    clients[i].addr = client_addr;
                    clients[i].last_alive = time(NULL);
                    clients[i].active = 1;
                    printf("Registered new client: %s\n", clients[i].name);
                    break;
                }
            }
            pthread_mutex_unlock(&clients_mutex);
            continue; // Don't process the name as a command
        }

        // Handle message from registered client
        handle_client_message(index, buffer);
    }
    return 0;
}
