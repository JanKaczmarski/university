CC      = gcc
CFLAGS  = -O2 -Wall -Wextra

.PHONY: all test clean

all: kotron

kotron: main.c build-dir
	$(CC) $(CFLAGS) -o build/$@ $<

test_kotron: test_kotron.c build-dir
	$(CC) $(CFLAGS) -o build/$@ $<

test: kotron test_kotron
	./build/test_kotron

build-dir:
	mkdir -p build

clean:
	rm -rf build/*
