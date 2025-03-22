CC=gcc
CFLAGS=-c -Wall
LDFLAGS=
BINARIES=countdown
BIN_DIR=bin
BUILD_DIR=build

.PHONY: all
all: main

# | - specify that depndency must exist before target is build, but
# isn't target isn't rebuilded (in this case countdown) when | dependency changes
main: main.o | $(BIN_DIR)
	$(CC) $(LDFLAGS) $(BUILD_DIR)/$< -o $(BIN_DIR)/$@

# $< is macro for getting first dependecy
# $@ is macro for getting target name
main.o: main.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) $< -o $(BUILD_DIR)/$@

$(BUILD_DIR):
	mkdir -p $@

$(BIN_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BIN_DIR) $(BUILD_DIR)
	rm -rf results
	find . -name "*.o" -type f -delete

