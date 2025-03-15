CC=gcc
CFLAGS=-c -Wall
LDFLAGS=
BINARIES=countdown
BIN_DIR=bin
BUILD_DIR=build
LIB_DIR=lib

.PHONY: all
all: mainDynamic mainStatic


mainDynamic: libDynamicMain | $(BIN_DIR)
	gcc -L$(LIB_DIR)  main.c -lmain -o $(BIN_DIR)/mainDynamic

libDynamicMain: main.o collatz.o | $(LIB_DIR)
	gcc -shared -o $(LIB_DIR)/libmain.so $(BUILD_DIR)/main.o $(BUILD_DIR)/collatz.o


# TODO: jk: Expand this to makros/variables
mainStatic: staticMainArchive | $(BIN_DIR)
	$(CC) main.c -L$(LIB_DIR) -lmain -o $(BIN_DIR)/mainStatic

staticMainArchive: main.o collatz.o | $(LIB_DIR)
	ar rc $(LIB_DIR)/libmain.a $(BUILD_DIR)/main.o $(BUILD_DIR)/collatz.o

collatz.o: collatz.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $(BUILD_DIR)/$@

main.o: main.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $(BUILD_DIR)/$@

$(BUILD_DIR):
	mkdir -p $@

$(LIB_DIR):
	mkdir -p $@

$(BIN_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BIN_DIR)
	rm -rf $(LIB_DIR)
	rm -rf $(BUILD_DIR)

