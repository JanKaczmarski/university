#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <sys/select.h>
#include <errno.h>

#define CMD_MEDIA "U "
#define CMD_MULTICAST "M "
#define MULTICAST_IP "239.0.0.1"
#define MULTICAST_PORT 9000
#define BUF_SIZE 2048

volatile sig_atomic_t keep_running = 1;

int sockStream = -1;
int sockDgram = -1;
int sockMulticast = -1;

struct sockaddr_in server_addr;
struct sockaddr_in mc_dest_addr;

void cleanup() {
    if (sockDgram != -1) {
        close(sockDgram);
        sockDgram = -1;
    }
    if (sockStream != -1) {
        close(sockStream);
        sockStream = -1;
    }
    if (sockMulticast != -1) {
        close(sockMulticast);
        sockMulticast = -1;
    }
}

void sigint_handler(int sig) {
    (void)sig; 
    keep_running = 0;
}

int setup_tcp(int port, const char *address) {
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr.s_addr = inet_addr(address);

    if (signal(SIGINT, sigint_handler) == SIG_ERR) {
        perror("signal");
        cleanup();
        return 1;
    }

    // Ignore SIGPIPE — prevents crash if server disconnects while client is mid-send()
    signal(SIGPIPE, SIG_IGN);

    sockStream = socket(AF_INET, SOCK_STREAM, 0);
    if (sockStream == -1) {
        perror("socket stream");
        return 1;
    }
    if (connect(sockStream, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1) {
        perror("connect");
        cleanup();
        return 1;
    }

    return 0;
}

int setup_udp() {
    struct sockaddr_in local_addr;
    socklen_t local_len = sizeof(local_addr);
    
    if (getsockname(sockStream, (struct sockaddr *)&local_addr, &local_len) == -1) {
        perror("getsockname failed");
        cleanup();
        return 1;
    }
    
    sockDgram = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockDgram == -1) {
        perror("socket dgram");
        return 1;
    }
    
    if (bind(sockDgram, (struct sockaddr *)&local_addr, sizeof(local_addr)) == -1) {
        perror("bind udp failed");
        cleanup();
        return 1;
    }

    return 0;
}

int setup_multicast() {
    sockMulticast = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockMulticast == -1) {
        perror("multicast socket failed");
        cleanup();
        return 1;
    }

    int opt_mc = 1;
    setsockopt(sockMulticast, SOL_SOCKET, SO_REUSEADDR, &opt_mc, sizeof(opt_mc));
    setsockopt(sockMulticast, SOL_SOCKET, SO_REUSEPORT, &opt_mc, sizeof(opt_mc));
    
    struct sockaddr_in mc_local_addr;
    memset(&mc_local_addr, 0, sizeof(mc_local_addr));
    mc_local_addr.sin_family = AF_INET;
    mc_local_addr.sin_port = htons(MULTICAST_PORT);
    mc_local_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(sockMulticast, (struct sockaddr *)&mc_local_addr, sizeof(mc_local_addr)) == -1) {
        perror("bind multicast failed");
        cleanup();
        return 1;
    }

    struct ip_mreq mreq;
    mreq.imr_multiaddr.s_addr = inet_addr(MULTICAST_IP);
    mreq.imr_interface.s_addr = htonl(INADDR_ANY);
    if (setsockopt(sockMulticast, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq)) == -1) {
        perror("multicast membership failed");
    }

    memset(&mc_dest_addr, 0, sizeof(mc_dest_addr));
    mc_dest_addr.sin_family = AF_INET;
    mc_dest_addr.sin_port = htons(MULTICAST_PORT);
    mc_dest_addr.sin_addr.s_addr = inet_addr(MULTICAST_IP);
    
    return 0;
}

int main(int argc, char *argv[]) {
    if (argc != 4) {
        printf("Usage: %s <server_ip> <server_port> <username>\n", argv[0]);
        return 1;
    }

    // validate user input
    char *endptr;
    long port = strtol(argv[2], &endptr, 10);
    if (*endptr != '\0' || port < 1 || port > 65535) {
        fprintf(stderr, "Invalid port number: %s (must be 1-65535)\n", argv[2]);
        return 1;
    }

    char *username = argv[3];

    if (setup_tcp((int)port, argv[1]) != 0) {
        printf("Failed to setup TCP channel!\n");
        return 1;
    }

    if (setup_udp() != 0) {
        printf("Failed to setup UDP channel!\n");
        return 1;
    }

    if (setup_multicast() != 0) {
        printf("Failed to setup multicast!\n");
        return 1;
    }

    printf("Connected to server!\n");
    fflush(stdout);

    fd_set read_fds;

    char buffer[BUF_SIZE];
    char send_buffer[BUF_SIZE];

    while (keep_running) {
        FD_ZERO(&read_fds);
        FD_SET(STDIN_FILENO, &read_fds); 
        FD_SET(sockStream, &read_fds); 
        FD_SET(sockDgram, &read_fds); 
        FD_SET(sockMulticast, &read_fds); 
        
        int max_fd = sockStream;
        if (sockDgram > max_fd) max_fd = sockDgram;
        if (sockMulticast > max_fd) max_fd = sockMulticast;

        if (select(max_fd + 1, &read_fds, NULL, NULL, NULL) == -1) {
            if (errno == EINTR) {
                if (!keep_running) break; // FIX #5: explicit check, same as server fix
                continue; 
            }
            perror("select failed");
            break;
        }

        if (FD_ISSET(sockStream, &read_fds)) {
            int bytes_received = recv(sockStream, buffer, sizeof(buffer) - 1, 0);
            if (bytes_received <= 0) {
                printf("\nServer disconnected or error.\n");
                break;
            }
            buffer[bytes_received] = '\0';

            printf("\r%s\n", buffer);
            printf("You: ");
            fflush(stdout);
        }

        if (FD_ISSET(sockDgram, &read_fds)) {
            int bytes_received = recvfrom(sockDgram, buffer, sizeof(buffer) - 1, 0, NULL, NULL);
            if (bytes_received <= 0) {
                printf("\nReceive from UDP socket failed.\n");
                continue;
            }
            buffer[bytes_received] = '\0';

            printf("\r[UDP Received]: %s\n", buffer);
            printf("You: ");
            fflush(stdout);
        }

        if (FD_ISSET(sockMulticast, &read_fds)) {
            int bytes_received = recvfrom(sockMulticast, buffer, sizeof(buffer) - 1, 0, NULL, NULL);
            if (bytes_received <= 0) {
                printf("\nReceive from Multicast socket failed.\n");
                continue;
            }
            buffer[bytes_received] = '\0';

            printf("\r[Multicast Received]: %s\n", buffer);
            printf("You: ");
            fflush(stdout);
        }

        if (FD_ISSET(STDIN_FILENO, &read_fds)) {
            if (fgets(buffer, sizeof(buffer) - 1, stdin) != NULL) {
                buffer[strcspn(buffer, "\n")] = 0;

                if (strlen(buffer) == 0) {
                    printf("You: ");
                    fflush(stdout);
                    continue;
                }

                if (strncmp(buffer, CMD_MEDIA, 2) == 0) { 
                    snprintf(send_buffer, sizeof(send_buffer), "[%s]: %s", username, buffer + 2);
                    if (sendto(sockDgram, send_buffer, strlen(send_buffer), MSG_NOSIGNAL, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1) {
                        perror("UDP sendto failed");
                    }
                } else if (strncmp(buffer, CMD_MULTICAST, 2) == 0) {
                    snprintf(send_buffer, sizeof(send_buffer), "[%s]: %s", username, buffer + 2);
                    if (sendto(sockMulticast, send_buffer, strlen(send_buffer), MSG_NOSIGNAL, (struct sockaddr *)&mc_dest_addr, sizeof(mc_dest_addr)) == -1) {
                        perror("Multicast sendto failed");
                    }
                } else { 
                    snprintf(send_buffer, sizeof(send_buffer), "[%s]: %s", username, buffer);
                    // MSG_NOSIGNAL prevents SIGPIPE crash on TCP send
                    if (send(sockStream, send_buffer, strlen(send_buffer), MSG_NOSIGNAL) == -1) {
                        perror("TCP send message failed");
                        break;
                    }
                }

                printf("You: ");
                fflush(stdout);
            }
        }
    }

    cleanup();
    printf("\nClient shutting down. Cleanup finished.\n");
    return 0;
}
