#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define MAX_NAME 32
#define BUFFER_SIZE 1024

struct sockaddr_in server_addr;
socklen_t server_addr_len = sizeof(server_addr);

/**
 * Global socket used for communication with the server.
 *
 * Initialized once in `main()` after creating the UDP socket.
 */
int sock;
/**
 * Client's name.
 *
 * Sent to the server immediately after startup.
 */
char name[MAX_NAME];
/**
 * Indicates whether the client is still running.
 *
 * Used to gracefully terminate background threads and stop message loops.
 * Can be modified by the SIGINT handler.
 */
volatile sig_atomic_t running = 1;

/**
 * Thread function that receives messages from the server.
 *
 * This function runs in the background and continuously reads messages from
 * the socket and prints them to stdout.
 *
 * @param arg Unused.
 * @return NULL
 */
void *recv_handler(void *arg)
{
    char buffer[BUFFER_SIZE];
    int len;
    struct sockaddr_in from_addr;
    socklen_t from_len = sizeof(from_addr);
    while (running)
    {
        if ((len = recvfrom(sock, buffer, BUFFER_SIZE - 1, 0, (struct sockaddr *)&from_addr, &from_len)) == -1)
        {
            perror("recvfrom");
        }
        else if (len > 0)
        {
            buffer[len] = '\0';
            printf("%s", buffer); fflush(stdout);
        }
    }
    return NULL;
}

/**
 * Handles SIGINT (Ctrl+C) signal to cleanly shut down the client.
 *
 * Sends a STOP message to the server, closes the socket, and terminates the process.
 *
 * @param sig Signal number (unused).
 */
void sigint_handler(int sig)
{
    running = 0;
    if (sendto(sock, "STOP", 4, 0, (struct sockaddr *)&server_addr, server_addr_len) == -1)
    {
        perror("sendto");
    }
    close(sock);
    printf("\nDisconnected from server.\n");
    exit(0);
}

/**
 * Thread function that periodically sends ALIVE messages to the server.
 *
 * This function helps the server know the client is still responsive.
 * The server may disconnect the client if it does not receive ALIVE messages regularly.
 *
 * @param arg Unused.
 * @return NULL
 */
void *alive_sender(void *arg)
{
    while (running)
    {
        sleep(5);
        if (sendto(sock, "ALIVE", 5, 0, (struct sockaddr *)&server_addr, server_addr_len) == -1)
        {
            perror("sendto");
        }
    }
    return NULL;
}

/**
 * Initializes the client and starts communication with the server.
 *
 * This function sets up the UDP socket, sends the client's name to the server,
 * and launches two background threads: one for handling incoming messages and another for
 * periodic health-checks. It also handles graceful shutdown on SIGINT.
 *
 * Expects exactly four arguments: client name, server IP, server Port, client Port.
 */
int main(int argc, char *argv[])
{
    if (argc != 5)
    {
        printf("Usage: %s <name> <IP> <SERVER_PORT> <PORT>\n", argv[0]);
        return 1;
    }

    strncpy(name, argv[1], MAX_NAME);

    sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock == -1)
    {
        perror("socket");
        return 1;
    }

    // Prepare server address
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(atoi(argv[3]));
    server_addr.sin_addr.s_addr = inet_addr(argv[2]);

    // use fixed-port for client
    struct sockaddr_in client_addr;
    client_addr.sin_family = AF_INET;
    client_addr.sin_port = htons(atoi(argv[4]));
    client_addr.sin_addr.s_addr = INADDR_ANY;

    if (bind(sock, (struct sockaddr *)&client_addr, sizeof(client_addr)) == -1)
    {
        perror("bind");
        return 1;
    }

    // Send client's name to the server
    if (sendto(sock, name, strlen(name), 0, (struct sockaddr *)&server_addr, server_addr_len) == -1)
    {
        perror("sendto");
        return 1;
    }

    signal(SIGINT, sigint_handler);

    pthread_t recv_thread, alive_thread;
    pthread_create(&recv_thread, NULL, recv_handler, NULL);
    pthread_create(&alive_thread, NULL, alive_sender, NULL);

    // Accepts messages from stdin and sends them to the server
    char buffer[BUFFER_SIZE];
    while (running)
    {
        fgets(buffer, BUFFER_SIZE, stdin);
        buffer[strcspn(buffer, "\n")] = '\0'; // Remove newline
        if (sendto(sock, buffer, strlen(buffer), 0, (struct sockaddr *)&server_addr, server_addr_len) == -1)
        {
            perror("sendto");
        }
    }

    pthread_join(recv_thread, NULL);
    pthread_join(alive_thread, NULL);
    return 0;
}
