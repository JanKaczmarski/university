#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <sys/wait.h>
#include <sys/time.h>

float func(float x) {
    return 4.0 / (x * x + 1);
}

float getRiemanValue(float (*func)(float), float a, float b) {
    return fabs(b - a) * func((a + b) / 2.0);
}

float get_time_ms() {
    struct timeval t;
    gettimeofday(&t, NULL);
    return (t.tv_sec * 1000.0) + (t.tv_usec / 1000.0);
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <delta_x> <max_processes>\n", argv[0]);
        return 1;
    }

    float delta_x = atof(argv[1]);
    int max_processes = atoi(argv[2]);

    for (int k = 1; k <= max_processes; k++) {
        int pipes[k][2];
        float total = 0.0;

        float start_time = get_time_ms();

        for (int i = 0; i < k; i++) {
            pipe(pipes[i]);

            if (fork() == 0) {
                close(pipes[i][0]); // close read end

                float a = (1.0 / k) * i;
                float b = a + (1.0 / k);
                float sum = 0.0;

                for (float x = a; x < b; x += delta_x) {
                    float next = x + delta_x;
                    if (next > b) next = b;
                    sum += getRiemanValue(func, x, next);
                }

                write(pipes[i][1], &sum, sizeof(float));
                close(pipes[i][1]);
                exit(0);
            }
        }

        // Parent: collect results
        for (int i = 0; i < k; i++) {
            close(pipes[i][1]); // close write end
            float part;
            read(pipes[i][0], &part, sizeof(float));
            close(pipes[i][0]);
            total += part;
        }

        for (int i = 0; i < k; i++) {
            wait(NULL);
        }

        float end_time = get_time_ms();
        printf("k = %d, wynik = %.10f, czas = %.2f ms\n", k, total, end_time - start_time);
    }

    return 0;
}
