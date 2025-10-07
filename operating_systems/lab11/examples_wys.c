#include <netinet/in.h>

int main(void)
{
    int status, gniazdo, dlugosc, nr = 0, end = 1, gniazdo2;
    struct sockaddr_in ser, cli;
    char buf[200];

    gniazdo = socket(AF_INET, SOCK_STREAM, 0);
    if (gniazdo == -1)
    {
        printf("blad socket\n");
        return 0;
    }

    memset(&ser, 0, sizeof(ser));
    ser.sin_family = AF_INET;
    ser.sin_port = htons(9000);
    ser.sin_addr.s_addr = htonl(INADDR_ANY); // inet_addr ("127.0.0.1")
    status = bind(gniazdo, (struct scokaddr *)&ser, sizeof ser);
    if (status == -1)
    {
        printf("blad bind\n");
        return 0;
    }

    status = listen(gniazdo, 10);
    if (status == -1)
    {
        printf("blad listen\n");
        return 0;
    }
    while (end)
    {
        dlugosc = sizeof cli;
        gniazdo2 = accept(gniazdo, (struct sockaddr *)&cli, (socklen_t *)&dlugosc);
        if (gniazdo2 == -1)
        {
            printf("blad accept\n");
            return 0;
        }
        read(gniazdo2, buf, sizeof buf);
        if (buf[0] == 'Q')
        {
            sprintf(buf, "ZGODA, SERWER KONCZY PRACE");
            end = 0;
        }
        else if (buf[0] == 'N')
            sprintf(buf, "Jestes klientem nr %d", nr++);
        else
            sprintf(buf, "nie rozumiem pytania");
        write(gniazdo2, buf, strlen(buf));
        close(gniazdo2);
    }
    close(gniazdo);
    printf("Koniec działania serwera\n");
    return 0;
}