#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>

int main(void)
{
    int status, sock, dlugosc, nr = 0, end = 1, sock2;
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
    ser.sin_addr.s_addr = htonl(INADDR_ANY); // inet_addr ("127.0.0.1")
    status = bind(sock, (struct sockaddr *)&ser, sizeof ser);
    if (status == -1)
    {
        printf("blad bind\n");
        return 0;
    }

    status = listen(sock, 10);
    if (status == -1)
    {
        printf("blad listen\n");
        return 0;
    }
    while (end)
    {
        dlugosc = sizeof cli;
        sock2 = accept(sock, (struct sockaddr *)&cli, (socklen_t *)&dlugosc);
        if (sock2 == -1)
        {
            printf("blad accept\n");
            return 0;
        }
        read(sock2, buf, sizeof buf);
        if (buf[0] == 'Q')
        {
            sprintf(buf, "ZGODA, SERWER KONCZY PRACE");
            end = 0;
        }
        else if (buf[0] == 'N')
            sprintf(buf, "Jestes klientem nr %d", nr++);
        else
            sprintf(buf, "nie rozumiem pytania");
        write(sock2, buf, strlen(buf));
        close(sock2);
    }
    close(sock);
    printf("Koniec działania serwera\n");
    return 0;
}
