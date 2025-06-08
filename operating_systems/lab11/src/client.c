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

/**
 * Global socket used for communication with the server.
 *
 * Initialized once in `main()` after a successful connection.
 */
int sock;
/**
 * Client's name.
 *
 * Sent to the server immediately after connection is established.
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
void *recv_handler(void *arg) {
    char buffer[BUFFER_SIZE];
    while (running) {
        int len = read(sock, buffer, BUFFER_SIZE - 1);
        if (len > 0) {
            buffer[len] = '\0';
            printf("%s", buffer);
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
void sigint_handler(int sig) {
    running = 0;
    write(sock, "STOP", 4);
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
void *alive_sender(void *arg) {
    while (running) {
        sleep(5);
        write(sock, "ALIVE", 5);
    }
    return NULL;
}

/**
 * Initializes the client and starts communication with the server.
 *
 * This function sets up the connection, registers the client's name, and launches
 * two background threads: one for handling incoming messages and another for
 * periodic health-checks. It also handles graceful shutdown on SIGINT.
 *
 * Assumes the server will close the connection if it doesn't receive regular ALIVE messages.
 *
 * Expects exactly three arguments: client name, server IP, and port.
 */
int main(int argc, char *argv[]) {
    if (argc != 4) {
        printf("Usage: %s <name> <IP> <PORT>\n", argv[0]);
        return 1;
    }

    strncpy(name, argv[1], MAX_NAME);

    // setup socket to commuincate with the server
    sock = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(atoi(argv[3]));
    server_addr.sin_addr.s_addr = inet_addr(argv[2]);

    if (connect(sock, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
        perror("connect");
        return 1;
    }

    write(sock, name, strlen(name));

    signal(SIGINT, sigint_handler);

    pthread_t recv_thread, alive_thread;
    pthread_create(&recv_thread, NULL, recv_handler, NULL);
    pthread_create(&alive_thread, NULL, alive_sender, NULL);

    // accepts messages from stdin and sends them to server
    char buffer[BUFFER_SIZE];
    while (running) {
        fgets(buffer, BUFFER_SIZE, stdin);
        write(sock, buffer, strlen(buffer));
    }

    pthread_join(recv_thread, NULL);
    pthread_join(alive_thread, NULL);
    return 0;
}
