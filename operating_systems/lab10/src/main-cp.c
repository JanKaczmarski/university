#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <stdbool.h>

#define PATIENTS_NUM 20
#define PHARMACISTS_NUM 3

#define MAX_PATIENT_IN_WAITING_ROOM 3
#define FIRST_AID_KIT_EMPTY_INDICATOR 3
#define FIRST_AID_KIT_CAP 6

int in_waiting_room, in_first_aid_kit;

struct timeval start_time;

typedef struct s_actor_args {
    int actor_id;
    int travel_time;
} actor_args;


pthread_mutex_t waiting_room_mutex;
pthread_cond_t waiting_room_cond;

pthread_mutex_t first_aid_kit_mutex;
pthread_cond_t first_aid_kit_cond;

pthread_cond_t doctor_cond;
pthread_mutex_t doctor_mutex;

int doctor_awake = 0;
int patients_ready[3]; // Pacjenci gotowi do konsultacji
int num_patients_waiting = 0;

int pharmacists_waiting = 0;
int patients_remaining = PATIENTS_NUM; // do zakończenia pracy lekarza

bool all_patients_treated = false;
pthread_mutex_t patients_mutex = PTHREAD_MUTEX_INITIALIZER;

int rand_range(int min, int max) {
    return rand() % (max - min + 1) + min;
}

double get_time_elapsed() {
    struct timeval now;
    gettimeofday(&now, NULL);

    double seconds = (now.tv_sec - start_time.tv_sec);
    double useconds = (now.tv_usec - start_time.tv_usec) / 1000000.0;

    return seconds + useconds;
}

void* patient_thread_work(void *arg) {
    actor_args casted_args = *(actor_args*)arg;
    int patient_id = casted_args.actor_id;
    int travel_time = casted_args.travel_time;
    // TODO: get random number for travel_time here!
    printf("[%0.10f] - Pacjent(%d): Ide do szpitala, bede za %d s\n", get_time_elapsed(), patient_id, travel_time);
    sleep(travel_time);

    pthread_mutex_lock(&waiting_room_mutex);
    while (in_waiting_room >= 3) {
        // TODO: get random number for travel time here!
        printf("[%0.10f] - Pacjent(%d): za duzo pacjentow, wracam za %d s\n", get_time_elapsed(), patient_id, travel_time);
        sleep(travel_time);
        pthread_cond_wait(&waiting_room_cond, &waiting_room_mutex);
    }
    in_waiting_room++;
    int pos_in_queue = in_waiting_room;
    pthread_mutex_unlock(&waiting_room_mutex);

    // TODO: Change this printf to logging func with params
    // float time_elapsed, int actor_id, char* message
    printf("[%0.10f] - Pacjent(%d): czeka %d pacjentow na lekarza\n", get_time_elapsed(), patient_id, in_waiting_room);

    if (pos_in_queue == 3) {
        printf("[%0.10f] - Pacjent(%d): budze lekarza\n", get_time_elapsed(), patient_id);
    }

    pthread_mutex_lock(&doctor_mutex);
    patients_ready[num_patients_waiting++] = patient_id;
    if (num_patients_waiting == 3) {
        pthread_cond_signal(&doctor_cond);
    }
    pthread_mutex_unlock(&doctor_mutex);


    pthread_mutex_lock(&doctor_mutex);
    patients_remaining--;
    pthread_cond_signal(&doctor_cond);
    pthread_mutex_unlock(&doctor_mutex);


    return NULL;
}

void* pharmacist_thread_work(void *arg) {
    actor_args casted_args = *(actor_args*)arg;
    int pharmacist_id = casted_args.actor_id;
    int travel_time = casted_args.travel_time;
    // TODO: get random number for travel_time here!
    while (patients_remaining > 0){
        printf("[%0.10f] - Farmaceuta(%d): Ide do szpitala, bede za %d s\n", get_time_elapsed(), pharmacist_id, travel_time);
        sleep(travel_time);

        while (in_first_aid_kit >= FIRST_AID_KIT_EMPTY_INDICATOR) {
            printf("[%0.10f] - Farmaceuta(%d): czekam na proznienie apteczki.\n", get_time_elapsed(), pharmacist_id);
            pthread_cond_wait(&first_aid_kit_cond, &first_aid_kit_mutex);
        }


        printf("[%0.10f] - Farmaceuta(%d): Budze lekarza\n", get_time_elapsed(), pharmacist_id);

        pthread_mutex_lock(&doctor_mutex);
        pharmacists_waiting++;

        printf("[%0.10f] - Farmaceuta(%d): dostarczam leki\n", get_time_elapsed(), pharmacist_id);


        // obudz lekarza
        pthread_cond_signal(&doctor_cond);
        pthread_mutex_unlock(&doctor_mutex);

        printf("[%0.10f] - Farmaceuta(%d): zakonczylem dostawe\n", get_time_elapsed(), pharmacist_id);
    }

    return NULL;
}

void* doctor_thread_work(void *arg) {
    while (1) {
        pthread_mutex_lock(&doctor_mutex);

        while (!(num_patients_waiting == 3 && in_first_aid_kit >= 3) &&
               !(pharmacists_waiting > 0 && in_first_aid_kit < FIRST_AID_KIT_EMPTY_INDICATOR) &&
               patients_remaining > 0) {
            pthread_cond_wait(&doctor_cond, &doctor_mutex);
        }

        if (patients_remaining == 0) {
            pthread_mutex_unlock(&doctor_mutex);
            break;
        }

        printf("[%0.10f] - Lekarz: budzę się\n", get_time_elapsed());

        if (num_patients_waiting == 3 && in_first_aid_kit >= 3) {
            printf("[%0.10f] - Lekarz: konsultuję pacjentów %d, %d, %d\n",
                   get_time_elapsed(), patients_ready[0], patients_ready[1], patients_ready[2]);

            sleep(rand() % 3 + 2); // 2–4s
            in_first_aid_kit -= 3;

            pthread_mutex_lock(&waiting_room_mutex);
            in_waiting_room -= 3;

            printf("[%0.10f] - Pacjent(%d): koncze wizyte\n", get_time_elapsed(), patients_ready[0]);
            printf("[%0.10f] - Pacjent(%d): koncze wizyte\n", get_time_elapsed(), patients_ready[1]);
            printf("[%0.10f] - Pacjent(%d): koncze wizyte\n", get_time_elapsed(), patients_ready[2]);

            pthread_cond_broadcast(&waiting_room_cond);
            pthread_mutex_unlock(&waiting_room_mutex);

            num_patients_waiting = 0;

            pthread_cond_broadcast(&doctor_cond);

        } else if (pharmacists_waiting > 0 && in_first_aid_kit < FIRST_AID_KIT_EMPTY_INDICATOR) {
            printf("[%0.10f] - Lekarz: przyjmuję dostawę leków\n", get_time_elapsed());
            sleep(rand() % 3 + 1); // 1–3s
            in_first_aid_kit = FIRST_AID_KIT_CAP;
            pharmacists_waiting--;
            pthread_cond_broadcast(&first_aid_kit_cond);
        }

        printf("[%0.10f] - Lekarz: zasypiam\n", get_time_elapsed());
        pthread_mutex_unlock(&doctor_mutex);
    }

    printf("[%0.10f] - Lekarz: kończy pracę\n", get_time_elapsed());
    all_patients_treated = true;
    return NULL;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <num_patients> <num_pharmacists>\n", argv[0]);
        return EXIT_FAILURE;
    }

    int num_patients = atoi(argv[1]);
    int num_pharmacists = atoi(argv[2]);

    pthread_t doctor_thread;
    pthread_t patient_threads[num_patients];
    pthread_t pharmacist_threads[num_pharmacists];

    srand(time(NULL));

    // start counting time
    gettimeofday(&start_time, NULL);

    pthread_mutex_init(&waiting_room_mutex, NULL);
    pthread_cond_init(&waiting_room_cond, NULL);
    pthread_mutex_init(&first_aid_kit_mutex, NULL);
    pthread_cond_init(&first_aid_kit_cond, NULL);
    pthread_mutex_init(&doctor_mutex, NULL);
    pthread_cond_init(&doctor_cond, NULL);

    pthread_create(&doctor_thread, NULL, doctor_thread_work, NULL);

    for (int i = 0; i < num_patients; i++) {
        actor_args *arg = malloc(sizeof(actor_args));
        arg->actor_id = i;
        arg->travel_time = rand_range(2, 5);
        pthread_create(&patient_threads[i], NULL, patient_thread_work, arg);
        sleep(1);
    }

    for (int i = 0; i < num_pharmacists; i++) {
        actor_args *arg = malloc(sizeof(actor_args));
        arg->actor_id = i;
        arg->travel_time = rand_range(5, 15);
        pthread_create(&pharmacist_threads[i], NULL, pharmacist_thread_work, arg);
        sleep(1);
    }

    for (int i = 0; i < num_patients; i++)
        pthread_join(patient_threads[i], NULL);

    pthread_join(doctor_thread, NULL);

    return EXIT_SUCCESS;
}
