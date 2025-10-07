#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>

int main(void)
{
    int status, sock;
    struct sockaddr_in ser, cli;
    char buf[200];

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == -1)
    {
        printf("blad socket\n");
        return 0;
    }

    memset(&ser, 0, sizeof(ser));
    ser.sin_family = AF_INET;
    ser.sin_port = htons(9000);
    ser.sin_addr.s_addr = inet_addr("127.0.0.1");

    status = connect(sock, (struct sockaddr *)&ser, sizeof ser);
    if (status < 0)
    {
        printf("blad connect\n");
        return 0;
    }
    printf("Podaj tekst: ");
    fgets(buf, sizeof buf, stdin);
    status = write(sock, buf, strlen(buf));
    status = read(sock, buf, (sizeof buf) - 1);
    buf[status] = '\0';
    printf("Otrzymalem: %s\n", buf);

    close(sock);
}
