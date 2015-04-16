#
# Makefile
#
# Copyright (C) 2015, Galois, Inc.
# All Rights Reserved.
#

PDF_FILES := GEC.pdf

all: $(PDF_FILES)

%.pdf: %.md
	@echo "PANDOC      $@"
	@pandoc -V documentclass=galois-proposal -t latex -o $@ $<

clean:
	rm -f $(PDF_FILES)

