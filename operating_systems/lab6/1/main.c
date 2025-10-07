#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <sys/wait.h>
#include <sys/time.h>

double func(double x) {
    return 4.0 / (x * x + 1);
}

double getRiemanValue(double (*func)(double), double a, double b) {
    return fabs(b - a) * func((a + b) / 2.0);
}

double get_time_ms() {
    struct timeval t;
    gettimeofday(&t, NULL);
    return (t.tv_sec * 1000.0) + (t.tv_usec / 1000.0);
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <delta_x> <max_processes>\n", argv[0]);
        return 1;
    }

    double delta_x = atof(argv[1]);
    int max_processes = atoi(argv[2]);

    for (int k = 1; k <= max_processes; k++) {
        int pipes[k][2];
        double total = 0.0;

        double start_time = get_time_ms();

        for (int i = 0; i < k; i++) {
            pipe(pipes[i]);

            if (fork() == 0) {
                close(pipes[i][0]);

                double a = (1.0 / k) * i;
                double b = a + (1.0 / k);
                double sum = 0.0;

                for (double x = a; x < b; x += delta_x) {
                    double next = x + delta_x;
                    if (next > b) next = b;
                    sum += getRiemanValue(func, x, next);
                }

                write(pipes[i][1], &sum, sizeof(double));
                close(pipes[i][1]);
                exit(0);
            }
        }

        // Parent: collect results
        for (int i = 0; i < k; i++) {
            close(pipes[i][1]);
            double part;
            read(pipes[i][0], &part, sizeof(double));
            close(pipes[i][0]);
            total += part;
        }

        for (int i = 0; i < k; i++) {
            wait(NULL);
        }

        double end_time = get_time_ms();
        printf("k = %d, wynik = %.10f, czas = %.2f ms\n", k, total, end_time - start_time);
    }

    return 0;
}
