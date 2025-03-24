#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int global = 0;

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Please provide path to directory, as first pos argument\n");
        return 1;
    }

    printf("%s\n", argv[0] + 2);

    char* dirpath = argv[1];

    int local = 0;
    pid_t pid, child_pid;
    int i, child_out;

    pid = fork();
    if (pid == 0) {
        printf("Child process\n");
        global++;
        local++;
        child_pid = getpid();
        printf("child pid = %d, parent pid = %d\n", child_pid, getppid());
        printf("child's local = %d, child's global = %d\n", local, global);
        child_out = execl("/bin/ls", "ls", dirpath, NULL);
        printf("Resulting error code: %d", child_out);
        return 0;
    }

    while(wait(0) > 0);

    printf("Parent process\n");
    printf("parent pid = %d, child pid = %d\n", getpid(), child_pid);
    printf("Child's exit code: %d\n", child_out);
    printf("Parent's local = %d, parent's global = %d\n", local, global);

    return 0;
}
