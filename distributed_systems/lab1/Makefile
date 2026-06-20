CC = gcc
CFLAGS = -Wall -Wextra -g
LDFLAGS = -pthread

# Zmienne ze wsparciem dla zmiennych środowiskowych (env)
PORT ?= 8000
IP ?= 127.0.0.1
NICK ?= Gosc

all: client server

client: client.c
	$(CC) $(CFLAGS) -o client client.c

server: server.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o server server.c

clean:
	rm -f client server
	rm -rf *dSYM

# Odpala serwer w aktualnym oknie (widzisz wszystkie logi)
runServer: server
	@echo "Odpalam serwer na porcie $(PORT)..."
	./server $(PORT)

# Odpala klienta w aktualnym oknie
runClient: client
	@echo "Odpalam klienta (Nick: $(NICK)) laczacego sie z $(IP):$(PORT)..."
	./client $(IP) $(PORT) $(NICK)
