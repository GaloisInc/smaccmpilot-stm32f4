# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-

EXEC = test/test

.PHONY: all $(EXEC) run clean

all: $(EXEC)

$(EXEC): test/hxstream-test.c  test/hxstream-test.h  test/ivory.h
	gcc -std=c99 -Wall test/hxstream-test.c -o $(EXEC)

run:
	$(EXEC)

clean:
	-rm -rf test


