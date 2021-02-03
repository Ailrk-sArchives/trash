CC = clang

ROOT_DIR = .
TEST_DIR = $(ROOT_DIR)/tests

CFLAGS = -std=c11
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -Wundef
CFLAGS += -g
CFLAGS += -Wno-ignored-qualifiers

TEST_LDFLAGS = -L$(TEST_DIR)
TESTLIB = -lunity

OBJS = ast.o util.o main.o

.PHONY: clean test

# all: objs
# 	$(CC) $(CFLAGS) -o $@ $<

all: o
	$(CC) $(CFLAGS) $(OBJS) -o $@

o: $(OBJS)

$(OBJS): makefile
#	$(CC) $(CFLAGS) $< -c -o $@

main.o: main.c ast.h util.h
ast.o: ast.c ast.h util.h
util.o: util.c util.h

clean:
	@rm ./*.o -f


