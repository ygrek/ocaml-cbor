.PHONY: build test clean install doc

all: build test

build:
	dune build @all

test: build
	dune runtest

clean:
	dune clean

install:
	dune install

doc:
	dune build @doc

VERSION=0.5
NAME=ocaml-cbor-$(VERSION)

.PHONY: release
release:
	git tag -a -m $(VERSION) $(VERSION)
	git archive --prefix=$(NAME)/ $(VERSION) | gzip > $(NAME).tar.gz
	gpg -a -b $(NAME).tar.gz > $(NAME).tar.gz.asc
