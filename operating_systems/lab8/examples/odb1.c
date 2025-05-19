#define ROZM_BLOKU 1024
int main(int argc, char **argv) {//POSIX
    int fd = shm_open("/nazwa_pam", O_CREATE | O_RDWR, 0644);
    ftruncate(fd, ROZM_BLOKU);
    char *wsk = (char *) mmap(NULL, ROZM_BLOKU, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    printf("Odczytano z pam. wspolnej: %s\n", wsk);
    munmap(wsk, ROZM_BLOKU);
    shm_unlik("/nazwa_pam");
    return 0;
}
