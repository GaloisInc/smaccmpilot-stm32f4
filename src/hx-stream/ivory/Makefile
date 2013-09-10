# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 8 -*-

.PHONY: all run clean

all: test

test: test/hxstream-test.c  test/hxstream-test.h  test/ivory.h
	gcc -std=c99 -Wall hxstream-test.c -o test

run:
	./test/test

clean:
	-rm -rf test


