#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>      // dla close()
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <sys/select.h>

int sock;
char buffer[1024];

void sigint_handler(int sig) {
    (void)sig; // int sig to satisfy func signature

    close(sock);
    printf("\nClient shutting down.\n");
    exit(0);
}

int main(int argc, char *argv[]) {
    // Odpalamy np: ./client 127.0.0.1 8080
    if (argc != 4) {
        printf("Usage: %s <server_ip> <server_port> <username>\n", argv[0]);
        return 1;
    }
    
    char *username = argv[3]; // Zapisujemy wskaźnik do nicka

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == -1) {
        perror("socket");
        return 1;
    }
    if (signal(SIGINT, sigint_handler) == SIG_ERR) {
        perror("signal");
        close(sock);
        return 1;
    }

    // Ustawienia adresu serwera, do którego się łączymy
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(atoi(argv[2]));
    server_addr.sin_addr.s_addr = inet_addr(argv[1]);

    if (connect(sock, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1) {
        perror("connect");
        close(sock);
        return 1;
    }

    printf("Connected to server! Start typing...\n");
    printf("You: ");
    fflush(stdout);

    fd_set read_fds;

    while (1) {
        FD_ZERO(&read_fds);
        FD_SET(STDIN_FILENO, &read_fds); // add stdin to select set
        FD_SET(sock, &read_fds); // add TCP sock to select set

        // select block until any fd from read_fds is ready to read
        if (select(sock + 1, &read_fds, NULL, NULL, NULL) == -1) {
            perror("select failed");
            break;
        }

        // socket
        if (FD_ISSET(sock, &read_fds)) {
            int bytes_received = recv(sock, buffer, sizeof(buffer) - 1, 0);
            if (bytes_received <= 0) {
                printf("\nServer disconnected or error.\n");
                break;
            }
            buffer[bytes_received] = '\0';

            printf("\r%s\n", buffer);
            printf("You: ");
            fflush(stdout);
        }

        // stdin
        if (FD_ISSET(STDIN_FILENO, &read_fds)) {
            if (fgets(buffer, sizeof(buffer) - 1, stdin) != NULL) {
                // fgets leaves \n at the end of message, replcae it with \0
                buffer[strcspn(buffer, "\n")] = 0;

                if (strlen(buffer) == 0) {
                    printf("You: ");
                    fflush(stdout);
                    continue;
                }

                char send_buffer[2048];
                snprintf(send_buffer, sizeof(send_buffer), "[%s]: %s", username, buffer);

                if (send(sock, send_buffer, strlen(send_buffer), 0) == -1) {
                    perror("send mesage failed");
                    break;
                }

                printf("You: ");
                fflush(stdout);
            }
        }
    }

    close(sock);
    return 0;
}
