# Deeplang Examples

This directory contains Deeplang (`.dp`) source files demonstrating various language features,
along with their compiled outputs in ANF (`.anf`) and WASM Text Format (`.wat`).

## File Index

| Source | ANF | WASM | Description |
|--------|-----|------|-------------|
| [basicMain.dp](basicMain.dp) | [basicMain.anf](basicMain.anf) | [basicMain.wat](basicMain.wat) | Basic variable declarations (`let`) with type annotations |
| [controlFlow.dp](controlFlow.dp) | [controlFlow.anf](controlFlow.anf) | [controlFlow.wat](controlFlow.wat) | `if`/`else` chains, `while` loops, `for` loops, `for`-`in` loops, `break`/`continue` |
| [example.dp](example.dp) | [example.anf](example.anf) | [example.wat](example.wat) | Comprehensive example: comments, ADT with methods, interface, impl with delegate, nested expressions |
| [interface.dp](interface.dp) | [interface.anf](interface.anf) | [interface.wat](interface.wat) | Interface definition, `impl` blocks, trait-based dispatch |
| [patternMatching.dp](patternMatching.dp) | [patternMatching.anf](patternMatching.anf) | [patternMatching.wat](patternMatching.wat) | Pattern matching: wildcard, variable, ADT, tuple, struct, literal patterns |
| [structAndADT.dp](structAndADT.dp) | [structAndADT.anf](structAndADT.anf) | [structAndADT.wat](structAndADT.wat) | ADT variants, struct with delegate (`as`), struct literal construction |

## Compilation Status

| File | Status | Notes |
|------|--------|-------|
| `basicMain.dp` | ✅ Compiles | Full pipeline: `.dp` → ANF → WAT |
| `controlFlow.dp` | ✅ Compiles | if/else, while, for, for-in, break/continue |
| `example.dp` | ✅ Compiles | Comprehensive: ADT with methods, interface, impl, pattern matching, expressions |
| `interface.dp` | ✅ Compiles | Interface definition, `impl` blocks, trait-based dispatch |
| `patternMatching.dp` | ✅ Compiles | ADT, tuple, struct, and literal pattern matching |
| `structAndADT.dp` | ✅ Compiles | ADT variants, struct with delegate, struct literal construction |

All files now parse and pass semantic analysis successfully. Each example is self-contained
with all referenced types, functions, and interfaces defined within the file itself.
The `.anf` and `.wat` files contain the full compiler output.

## Pipeline

Each `.dp` file is processed through the compiler pipeline:

```
.dp source → Lexer → Parser → AST → Walker (semantic analysis) → ANF IR → WAT codegen
```

- **`.anf`** — A-Normal Form intermediate representation with CPS-style blocks, labels, and jumps
- **`.wat`** — WebAssembly Text Format (S-expression), targeting a stack-based VM with a bump allocator

## Compilation Example

**Input** (`basicMain.dp`):
```deeplang
fun main(x: Char) {
  let x : I32 = 1;
  let a: I32 = 2;
  let b: F32 = 3.0;
}
```

**ANF** (`basicMain.anf`):
```
fun main($1) -> #1 =
  $2 = 1
  $3 = 2
  $4 = 3.000000
  jump #1 ()
```

**WAT** (`basicMain.wat`):
```lisp
(func $main (param $v1 i32) (result i32)
  i32.const 1
  local.set $v2
  i32.const 2
  local.set $v3
  f32.const 3.
  local.set $v4
  br $l1
  (block $l1 (nop)))
```

## Running

```bash
# Build the compiler
dune build

# Compile a single .dp file and view ANF + WAT
dune exec IR/test/WasmGenTest.exe -- examples/basicMain.dp

# Just the ANF
dune exec IR/test/ConversionTest.exe -- examples/basicMain.dp
```
