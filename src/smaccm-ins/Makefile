CC = gcc
CFLAGS = -Wall -Os -Igen
CODEGEN = dist/build/codegen/codegen

all: gen/psas

$(CODEGEN):
	cabal build codegen

-include gen/deps
gen/deps: $(CODEGEN)
	$(CODEGEN) --include-dir=gen --src-dir=gen --dep-prefix=FILTER --dep-file=$@

$(FILTER_SOURCES) $(FILTER_HEADERS): $(CODEGEN)
	$(CODEGEN) --include-dir=gen --src-dir=gen --const-fold

gen/%: tests/%.o $(addsuffix .o, $(basename $(FILTER_SOURCES)))
	$(CC) -o $@ $^ -lm

clean:
	rm -rf gen

.PHONY: $(CODEGEN) clean
