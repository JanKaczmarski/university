#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#define A 0.0
#define B 1.0

double delta_x;
int num_threads;
double result = 0.0;
pthread_mutex_t mutex;

double func(double x) {
    return 4.0 / (x * x + 1);
}

double getRiemannValue(double a, double b) {
    return fabs(b - a) * func((a + b) / 2.0);
}

void* thread_work(void* arg) {
    // thread identifier, says which slice to compute
    int i = *(int*)arg;
    double local_a = A + i * (B - A) / num_threads;
    double local_b = A + (i + 1) * (B - A) / num_threads;
    double sum = 0.0;

    for (double x = local_a; x < local_b; x += delta_x) {
        double next = x + delta_x;
        if (next > local_b) next = local_b;
        sum += getRiemannValue(x, next);
    }

    pthread_mutex_lock(&mutex);
    result += sum;
    pthread_mutex_unlock(&mutex);

    return NULL;
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        printf("Usage: %s <delta_x> <num_threads>\n", argv[0]);
        return 1;
    }

    delta_x = atof(argv[1]);
    num_threads = atoi(argv[2]);

    pthread_t* threads = malloc(num_threads * sizeof(pthread_t));
    // array storing thread_work arguments
    int* indices = malloc(num_threads * sizeof(int));
    pthread_mutex_init(&mutex, NULL);

    for (int i = 0; i < num_threads; i++) {
        indices[i] = i;
        pthread_create(&threads[i], NULL, thread_work, &indices[i]);
    }

    for (int i = 0; i < num_threads; i++) {
        pthread_join(threads[i], NULL);
    }

    printf("Wynik: %.10f\n", result);

    pthread_mutex_destroy(&mutex);
    free(threads);
    free(indices);
    return 0;
}
