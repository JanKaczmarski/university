#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define LOG(fmt, ...) fprintf(stderr, "[%s] " fmt "\n", program_name, ##__VA_ARGS__)


int main() {
    const char* program_name = "master";
    const char* fifo_to_worker = "to_worker";
    const char* fifo_to_master = "to_master";

    float a, b;

    if (isatty(0)) {
        LOG("Podaj przedział (a b): ");
    }

    if (fscanf(stdin, "%f %f", &a, &b) != 2) {
        LOG("Błąd: podaj poprawny przedział w formacie: <a> <b>");
        exit(1);
    }


    LOG("Sending range [%f, %f] to '%s'", a, b, fifo_to_worker);

    int out_fd = open(fifo_to_worker, O_WRONLY);
    write(out_fd, &a, sizeof(float));
    write(out_fd, &b, sizeof(float));
    close(out_fd);

    float result;
    int in_fd = open(fifo_to_master, O_RDONLY);
    read(in_fd, &result, sizeof(float));
    close(in_fd);

    LOG("Received result: %f", result);

    unlink(fifo_to_worker);
    unlink(fifo_to_master);

    return 0;
}
