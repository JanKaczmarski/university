
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_INPUT 100

int main(int argc, char **argv) {
    void *handle = dlopen("lib/libmain.so", RTLD_LAZY);
    if(!handle){
        printf("Failed to open lib/libmain.so file for dynamic lib load\n");
        return 1;
    }

    void (*lib_fun)(int input, int max_iter, int *steps);
    lib_fun = (void (*)())dlsym(handle,"test_collatz_convergence");

    if(dlerror() != NULL){
        printf("Failed to import library function test_collatz_convergence\n");
        return 1;
    }

    if (argc < 2) {
        printf("ERROR: Please provide `int input` as first program argument!\n");
        return 1;
    }

    int input = atoi(argv[1]);
    int i = 0;


    (*lib_fun)(input, MAX_INPUT, &i);

    dlclose(handle);
}

