//sem1.c

#include <stdio.h>
#include <fcntl.h>
#include <semaphore.h>

int main(void) {// Semafory POSIX
    sem_t *id_sem = sem_open("nazwa_sem", O_CREAT, 0666, NULL);
    pid_t pid1 = fork();
    if (pid1 == 0){
        sleep(1);
        printf("czekam potomny\n");
        sem_wait(id_sem);
        printf("wewnatrz potomny\n");
        sleep(3);
        sem_post(id_sem);
        printf("po potomny\n");
        return 0;
    } else {
        sleep(2);
        printf("czekam glowny\n"); fflush(stdout);
        sem_wait(id_sem);
        printf("wewnatrz glowny\n"); fflush(stdout);
        sleep(2);
        sem_post(id_sem);
        printf("po glowny\n"); fflush(stdout);
        return 0;
    }
    sem_close(id_sem);
    return 0;
}

