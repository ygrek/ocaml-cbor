.PHONY: build test clean

all: build test

build:
	dune build @all

test:
	dune runtest

clean:
	dune clean

VERSION=$(shell oasis query version)
NAME=ocaml-cbor-$(VERSION)

.PHONY: release
release:
	git tag -a -m $(VERSION) $(VERSION)
	git archive --prefix=$(NAME)/ $(VERSION) | gzip > $(NAME).tar.gz
	gpg -a -b $(NAME).tar.gz
