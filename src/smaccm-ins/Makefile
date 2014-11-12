CC = gcc
CFLAGS = -Wall -Os -Igen
FILTER_SOURCES = gen/smaccm_ins.c

all: gen/psas

$(FILTER_SOURCES):
	cabal run --only codegen -- --src-dir=gen --const-fold

gen/%: tests/%.o $(addsuffix .o, $(basename $(FILTER_SOURCES)))
	$(CC) -o $@ $^ -lm

clean:
	rm -rf gen

.PHONY: $(FILTER_SOURCES) clean
