#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_matrix.h>

// ! To compile this file use following compilation flags
// gcc -o bench main.c -I/opt/homebrew/include/ -L/opt/homebrew/lib/ -lgsl -lgslcblas -lm

void naive_multiplication(double **A, double **B, double **C, int N, int M, int P);
void better_multiplication(double **A, double **B, double **C, int N, int M, int P);

void allocMatrix(double ***matrix, int n, int m);
void freeMatrix(double ***matrix, int n);

void naive_multiplication(double **A, double **B, double **C, int N, int M, int P)
{
    // N: number of rows in A
    // M: number of columns in A (and rows in B)
    // P: number of columns in B

    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < P; j++)
        {
            C[i][j] = 0.0;
        }
    }

    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < P; j++)
        {
            for (int k = 0; k < M; k++)
            {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }
}

void better_multiplication(double **A, double **B, double **C, int N, int M, int P)
{
    // N: number of rows in A
    // M: number of columns in A (and rows in B)
    // P: number of columns in B
    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < P; j++)
        {
            C[i][j] = 0.0;
        }
    }

    for (int i = 0; i < N; i++)
    {
        for (int k = 0; k < M; k++)
        {
            for (int j = 0; j < P; j++)
            {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }
}

void blas_multiplication(double **A, double **B, double **C, int N, int M, int P)
{
    gsl_matrix *gslA = gsl_matrix_alloc(N, M);
    gsl_matrix *gslB = gsl_matrix_alloc(M, P);
    gsl_matrix *gslC = gsl_matrix_alloc(N, P);

    // Copy A
    for (int i = 0; i < N; i++)
        for (int j = 0; j < M; j++)
            gsl_matrix_set(gslA, i, j, A[i][j]);

    // Copy B
    for (int i = 0; i < M; i++)
        for (int j = 0; j < P; j++)
            gsl_matrix_set(gslB, i, j, B[i][j]);

    // Perform C = A * B
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, gslA, gslB, 0.0, gslC);

    // Copy result to C
    for (int i = 0; i < N; i++)
        for (int j = 0; j < P; j++)
            C[i][j] = gsl_matrix_get(gslC, i, j);

    gsl_matrix_free(gslA);
    gsl_matrix_free(gslB);
    gsl_matrix_free(gslC);
}

void allocMatrix(double ***matrix, int n, int m)
{
    *matrix = (double **)malloc(n * sizeof(double *));
    for (int i = 0; i < n; i++)
    {
        (*matrix)[i] = (double *)malloc(m * sizeof(double));
    }
}

void freeMatrix(double ***matrix, int n)
{
    for (int i = 0; i < n; i++)
    {
        free((*matrix)[i]);
    }
    free(*matrix);
}

void fillMatrix(double **matrix, int n, int m)
{
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            matrix[i][j] = rand() % 10;
}

int main()
{
    FILE *file = fopen("benchmark_results_test.csv", "w");
    if (!file)
    {
        perror("Cannot open file");
        return 1;
    }

    fprintf(file, "Size,Naive,Better,BLAS\n");

    srand(time(NULL));

    for (int size = 2; size <= 502; size += 50)
    {
        for (int sample = 0; sample < 10; sample++)
        {
            double **A, **B, **C;
            allocMatrix(&A, size, size);
            allocMatrix(&B, size, size);
            allocMatrix(&C, size, size);

            fillMatrix(A, size, size);
            fillMatrix(B, size, size);

            clock_t begin, end;
            double time_naive, time_better, time_blas;

            // Time naive
            begin = clock();
            naive_multiplication(A, B, C, size, size, size);
            end = clock();
            time_naive = (double)(end - begin) / CLOCKS_PER_SEC;

            // Time better
            begin = clock();
            better_multiplication(A, B, C, size, size, size);
            end = clock();
            time_better = (double)(end - begin) / CLOCKS_PER_SEC;

            // Time BLAS
            begin = clock();
            blas_multiplication(A, B, C, size, size, size);
            end = clock();
            time_blas = (double)(end - begin) / CLOCKS_PER_SEC;

            fprintf(file, "%d,%f,%f,%f\n", size, time_naive, time_better, time_blas);

            freeMatrix(&A, size);
            freeMatrix(&B, size);
            freeMatrix(&C, size);
        }
    }

    fclose(file);
    return 0;
}
