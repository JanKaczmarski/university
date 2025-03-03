CC=gcc
CFLAGS=-c -Wall -std=c17

.PHONY: all
all: countdown.o

.PHONY: countdown.o
countdown.o: countdown.c
	$(CC) $(CFLAGS) -c countdown.c

.PHONY: clean
clean:
	find . -name "*.o" -type f -delete


