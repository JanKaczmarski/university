CC      = gcc
CFLAGS  = -O2 -Wall -Wextra

.PHONY: all test clean

all: kotron

kotron: main.c build-dir
	$(CC) $(CFLAGS) -o build/$@ $<

kotron2: main2.c build-dir
	$(CC) $(CFLAGS) -o build/$@ $<

test_kotron: test_kotron.c build-dir
	$(CC) $(CFLAGS) -o build/$@ $<

test: kotron test_kotron
	./build/test_kotron

test2: kotron2 test_kotron
	sed 's|./build/kotron|./build/kotron2|g' test_kotron.c > /tmp/test_kotron2.c
	$(CC) $(CFLAGS) -o build/test_kotron2 /tmp/test_kotron2.c
	./build/test_kotron2

build-dir:
	mkdir -p build

clean:
	rm -rf build/*
