## Deeplang Type System

We are learning TAPL and developing some interesting projects as follow:

- [untyped-lambda-calculus](http://mepy.net/untyped-lambda-calculus/) based on javascript
- [Dart-Lambda-Calculus](https://github.com/sorrowfulT-Rex/Dart-Lambda-Calculus) based on Dart

**Todo list:**

- Deeplang parser/lexer
- Deeeplang typing rules
- Deeplang symbol table
- Deeplang type checker and then type infer
- Deeplang wasm codegen


### Building
Currently, only the AST definition is completed.
Its source file is at `parser/ParseTree.ml`.
To build the AST definition,
you need an OCaml compiler and the [dune](https://dune.build/) build system.
To build directly with dune, do:
```
dune build
```
You can also build with Makefile, via:
```
make build
```
To build the module document of source files as well,
you need to install the [odoc](https://github.com/ocaml/odoc) document generator as well.
Once `odoc` is installed, you can build module documents through:
```
make doc
```
The generated documents of internal modules
are located in `doc/internal/module_name-xxxxxxxxxx`,
in HTML format.
You can preview the HTML online by opening the link of the HTML file
in [](htmlpreview.github.io)

To build source files and document together, do:
```
make all # or simply `make`
```


### Development Guide
To add new OCaml modules,
you just need to modify the `dune` build file,
and add your modules/libraries/executables into it.
You can find the document of dune [here](https://dune.readthedocs.io/en/stable/overview.html).
