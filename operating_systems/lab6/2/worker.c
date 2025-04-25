#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <fcntl.h>

#define LOG(fmt, ...) fprintf(stderr, "[%s] " fmt "\n", program_name, ##__VA_ARGS__)
#define DELTA_X 0.0000001

float func(float x) {
    return 4.0 / (x * x + 1);
}

float getRiemanValue(float (*func)(float), float a, float b) {
    return fabs(b - a) * func((a + b) / 2.0);
}

int main() {
    const char* program_name = "worker";
    const char* fifo_to_worker = "to_worker";
    const char* fifo_to_master = "to_master";

    LOG("Waiting for range on '%s'", fifo_to_worker);
    float a, b;
    int in_fd = open(fifo_to_worker, O_RDONLY);
    read(in_fd, &a, sizeof(float));
    read(in_fd, &b, sizeof(float));
    close(in_fd);

    LOG("Received range: [%f %f]", a, b);
    LOG("Computing integral...");

    float sum = 0.0;

    for (float x = a; x < b; x += DELTA_X) {
        float next = x + DELTA_X;
        if (next > b) next = b;
        sum += getRiemanValue(func, x, next);
    }

    LOG("Sending results to '%s'", fifo_to_master);

    int out_fd = open(fifo_to_master, O_WRONLY);
    write(out_fd, &sum, sizeof(float));
    close(out_fd);

    return 0;
}
