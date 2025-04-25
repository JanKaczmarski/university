#include <stdio.h>
#include <stdlib.h>

#define MAX_INPUT 100

#ifdef DYNAMIC_MODE
#include <dlfcn.h>
#endif


#ifndef DYNAMIC_MODE
// Declare the function normally — linked statically or via shared library
extern void test_collatz_convergence(int input, int max_iter, int *steps);
#endif

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("ERROR: Please provide `int input` as first program argument!\n");
        return 1;
    }

    int input = atoi(argv[1]);
    int i = 0;

#ifdef DYNAMIC_MODE
    void *handle = dlopen("lib/libmain.so", RTLD_LAZY);
    if (!handle) {
        printf("Failed to open lib/libmain.so file for dynamic lib load\n");
        return 1;
    }

    void (*lib_fun)(int, int, int *) = (void (*)(int, int, int *))dlsym(handle, "test_collatz_convergence");

    if (dlerror() != NULL || lib_fun == NULL) {
        printf("Failed to import library function test_collatz_convergence\n");
        dlclose(handle);
        return 1;
    }

    lib_fun(input, MAX_INPUT, &i);
    dlclose(handle);
#else
    test_collatz_convergence(input, MAX_INPUT, &i);
#endif

    return 0;
}
