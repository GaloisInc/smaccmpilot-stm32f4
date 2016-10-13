ELM=elm-make
CLOSURE=closure-compiler
CLOSURE_FLAGS=-O ADVANCED --language_in ECMASCRIPT5 --jscomp_off globalThis

ELM_SRC=./src
STATIC_JS=../web

default: elm

elm: $(STATIC_JS)/elm.min.js

$(STATIC_JS)/elm.min.js: elm.js
	$(CLOSURE) $(CLOSURE_FLAGS) --js elm.js --js_output_file $(STATIC_JS)/elm.min.js

elm.js: $(ELM_SRC)/*.elm
	$(ELM) $(ELM_SRC)/Main.elm --output $@

.PHONY: clean

clean:
	rm elm.js
