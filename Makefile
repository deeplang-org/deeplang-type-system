.PHONY: all
all: build doc

.PHONY: build
build:
	dune build

.PHONY: doc
doc: build
	dune build @doc-private
	cp -r _build/default/_doc/_html/* doc/internal/


.PHONY: clean
clean:
	dune clean
