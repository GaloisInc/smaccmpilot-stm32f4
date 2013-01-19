
# We're running ivory and the smaccm-mavlink tools from a cabal-dev sandbox

TOP=..
DSL=$(TOP)/../dsl
SANDBOX=$(DSL)/cabal-dev
SMACCM_MAVLINK=$(DSL)/smaccm-mavlink

INCDIR = ./include/smavlink/messages
SRCDIR = ./src/messages

INFO = "use `make regenerate` to regenerate smavlink message sources from ivory"

default:
	echo $(INFO)

clean:
	-rm $(INCDIR)/*.h
	-rm $(SRCDIR)/*.c

regenerate:
	$(SANDBOX)/bin/smavlink-gen-c --include-dir $(INCDIR) \
		--src-dir $(SRCDIR)

