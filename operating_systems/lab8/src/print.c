#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mqueue.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <semaphore.h>
#include <sys/mman.h>

#define SERVER_QUEUE_NAME "/print_queue"
#define MAX_MSG_SIZE 256
#define NUMBER_OF_PRINTERS 3
#define SEM_NAME "/my_sem"
#define MUTEX_NAME "/id_mutex"
#define SHM_NAME "/printer_counter"

int main() {
    // Cleanup previous resources
    sem_unlink(SEM_NAME);
    sem_unlink(MUTEX_NAME);
    shm_unlink(SHM_NAME);

    // Setup printer control semaphore
    sem_t *sem = sem_open(SEM_NAME, O_CREAT | O_RDWR, 0644, NUMBER_OF_PRINTERS);
    if (sem == SEM_FAILED) {
        perror("sem_open");
        return 1;
    }

    printf("Successfully created printer semaphore!\n");

    // Setup mutex for shared counter
    sem_t *mutex = sem_open(MUTEX_NAME, O_CREAT | O_RDWR, 0644, 1);
    if (mutex == SEM_FAILED) {
        perror("sem_open (mutex)");
        return 1;
    }

    // Shared memory setup
    int shm_fd = shm_open(SHM_NAME, O_CREAT | O_RDWR, 0666);
    if (shm_fd == -1) {
        perror("shm_open");
        return 1;
    }

    if (ftruncate(shm_fd, sizeof(int)) == -1) {
        perror("ftruncate");
        return 1;
    }

    int *printer_id_counter = mmap(NULL, sizeof(int), PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
    if (printer_id_counter == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    // Reset counter initially
    *printer_id_counter = 0;

    // Message queue setup
    struct mq_attr attr = {
        .mq_flags = 0,
        .mq_maxmsg = 10,
        .mq_msgsize = MAX_MSG_SIZE,
        .mq_curmsgs = 0
    };

    mqd_t print_qd = mq_open(SERVER_QUEUE_NAME, O_CREAT | O_RDONLY, 0666, &attr);
    if (print_qd == -1) {
        perror("mq_open");
        return 1;
    }

    printf("Print server is ready to receive jobs.\n");

    // Main loop
    char buffer[MAX_MSG_SIZE];
    while (1) {
        if (sem_wait(sem) == -1) {
            perror("sem_wait");
            return 1;
        }

        printf("SYSTEM: Waiting for print order...\n");

        if (mq_receive(print_qd, buffer, MAX_MSG_SIZE, NULL) == -1) {
            perror("mq_receive");
            continue;
        }

        printf("SYSTEM: Received print job: %s\n", buffer);

        if (fork() == 0) {
            // Get printer ID
            if (sem_wait(mutex) == -1) {
                perror("sem_wait (mutex)");
                exit(1);
            }

            int my_id = (*printer_id_counter) % NUMBER_OF_PRINTERS;
            *printer_id_counter = (*printer_id_counter + 1) % NUMBER_OF_PRINTERS;
            sem_post(mutex);

            // Simulate printing
            for (int i = 0; i < MAX_MSG_SIZE && buffer[i] != '\0'; i++) {
                printf("PRINTER %d: %c\n", my_id % 3, buffer[i]);
                sleep(1);
            }
            printf("PRINTER %d FINISHED PRINTING: %s\n", my_id % 3, buffer);

            // Mark printer as free
            sem_post(sem);
            exit(0);
        }
    }

    return 0;
}
