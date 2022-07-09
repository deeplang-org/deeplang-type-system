.PHONY: all
all: build doc

.PHONY: build
build:
	dune build

.PHONY: doc
doc: build
	dune build @doc-private
	rm -Rf doc/internal/*
	cp -r _build/default/_doc/_html/* doc/internal/

.PHONY: test
test: build
	dune test

.PHONY: clean
clean:
	dune clean
