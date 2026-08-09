## Deeplang Type System

We are learning TAPL and developing some interesting projects as follow:

- [untyped-lambda-calculus](http://mepy.net/untyped-lambda-calculus/) based on javascript
- [Dart-Lambda-Calculus](https://github.com/sorrowfulT-Rex/Dart-Lambda-Calculus) based on Dart

### Current Status

The Deeplang compiler frontend has been implemented with the following pipeline:

```
Source (.dp) → Lexer → Parser → Semantic Walker → ANF Conversion → WAT Code Generation
```

| Module | Status | Description |
|--------|--------|-------------|
| [Syntax/](Syntax/) | ✅ 完成 | 词法分析器 (Lexer)、语法解析器 (Parser)、AST 定义 (ParseTree) |
| [Semantics/](Semantics/) | ✅ 完成 | 符号表 (Table)、类型辅助 (Helper)、语义遍历 (Walker) |
| [IR/ANF.ml](IR/ANF.ml) | ✅ 完成 | A-Normal Form IR 定义，含 CPS 风格的 block/label/jump |
| [IR/Conversion.ml](IR/Conversion.ml) | ✅ 完成 | AST → ANF 转换，基于 continuation-passing style |
| [IR/ConvertMatch.ml](IR/ConvertMatch.ml) | ✅ 完成 | 模式匹配 (pattern matching) 到 ANF branching 的转换 |
| [IR/WasmGen.ml](IR/WasmGen.ml) | ✅ 完成 | ANF → WASM Text Format (.wat) 代码生成，含 bump allocator 内存模型 |

**已实现的语言特性：**

- 基础类型：`unit`, `bool`, `int`, `float`, `char`
- 复合类型：struct、ADT (algebraic data types)、tuple、interface
- 表达式：字面量、二元/一元运算、函数调用、struct 字段访问、方法调用、`this`
- 语句：变量声明（含模式解构）、赋值（含复合赋值 `+=` 等）、`if/else`、`match`、`for`、`return`、`break`、`continue`
- Interface 继承 (`extends`) 及方法表合并
- 模式匹配：通配符、变量、struct、ADT、tuple、or 模式
- ANF 的 FieldByName 延迟解析：字段名在代码生成前通过预遍历解析为字段索引

### Prepare
- ocaml >= 4.12.0
- dune >= 2.8

### Building
To build the project, you need an OCaml compiler and the [dune](https://dune.build/) build system.
```
dune build
```
You can also build with Makefile, via:
```
make build
```

### Testing
```
dune test
# 或
dune build @runtest
```

测试覆盖：
- **Semantics/test** — 语义分析测试 (WalkerTest)，覆盖表达式、函数、模式匹配、语句、类型
- **IR/test** — IR 转换测试 (ConversionTest: AST→ANF) 和 WAT 代码生成测试 (WasmGenTest: AST→ANF→WAT)

### Documentation
To build the module document of source files,
you need to install the [odoc](https://github.com/ocaml/odoc) document generator.
Once `odoc` is installed, you can build module documents through:
```
make doc
```
The generated documents of internal modules
are located in `doc/internal/module_name-xxxxxxxxxx`,
in HTML format.

### Development Guide
To add new OCaml modules,
modify the `dune` build file and add your modules/libraries/executables.
Dune documentation: [dune.readthedocs.io](https://dune.readthedocs.io/en/stable/overview.html).
