<h1 align="center">Deeplang Type System</h1>

<p align="center">
  <img alt="build" src="https://img.shields.io/badge/build-passing-brightgreen.svg">
  <img alt="open source" src="https://img.shields.io/badge/project-open-brightgreen.svg">
  <a href="#license"><img alt="license" src="https://img.shields.io/badge/license-MIT-green.svg"></a>
  <img alt="version" src="https://img.shields.io/badge/version-v1.0.0-blue.svg">
  <img alt="OCaml" src="https://img.shields.io/badge/OCaml-4.14.1-orange.svg">
  <img alt="dune" src="https://img.shields.io/badge/dune-3.24.2-blueviolet.svg">
  <img alt="WebAssembly" src="https://img.shields.io/badge/WebAssembly-1.0-654ff0.svg">
</p>

## deepc 编译前端

`deepc` 是 Deeplang 的编译前端，将源码 `.dp` 完整编译为 WebAssembly 1.0 文本格式（`.wat`），也可选择输出 A-Normal Form 中间表示（`.anf`）。

```bash
# 编译为 WAT（默认）
deepc examples/basicMain.dp         # 生成 examples/basicMain.wat

# 输出 ANF 中间表示
deepc --anf examples/basicMain.dp   # 生成 examples/basicMain.anf

# 查看版本信息
deepc --version                     # 或 -v
```

构建后，可执行文件位于 `_build/default/deepc`（Linux）或 `_build/default/deepc.exe`（Windows），也可以用 `dune exec deepc -- <file.dp>` 直接运行。

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

### 环境准备

**安装 OCaml 开发环境 (推荐使用 opam)：**

```bash
# 安装 opam (OCaml 包管理器)
# Linux (x86_64):
wget https://github.com/ocaml/opam/releases/download/2.5.2/opam-2.5.2-x86_64-linux -O ~/.local/bin/opam
chmod +x ~/.local/bin/opam

# macOS:
brew install opam

# 初始化 opam
opam init --disable-sandboxing

# 创建 OCaml 4.14.1 开发环境
opam switch create ocaml414 4.14.1
eval $(opam env)

# 安装依赖
opam install dune ppx_deriving odoc -y
```

**版本要求：**

| 工具 | 最低版本 | 当前开发版本 |
|------|----------|-------------|
| OCaml | ≥ 4.12.0 | 4.14.1 |
| dune  | ≥ 2.8   | 3.24.2 |
| opam  | ≥ 2.1   | 2.5.2 |

### Building

```bash
# 激活 opam 环境（每次新终端都需要）
eval $(opam env)

# 构建项目
dune build

# 或使用 Makefile
make build
```

**项目模块结构：**

```
.
├── Syntax/          # 词法分析 & 语法解析
│   ├── ParseTree.ml    # AST 定义
│   ├── Parser.mly      # Menhir/ocamlyacc 语法规则
│   ├── Lexer.mll       # ocamllex 词法规则
│   └── SyntaxError.ml  # 语法错误类型
├── Semantics/       # 语义分析
│   ├── Table.ml        # 符号表 (变量/函数/类型/ADT表)
│   ├── Helper.ml       # 类型辅助 (相等、漂亮打印)
│   ├── Walker.ml       # AST Walker (语义遍历)
│   └── SemanticsError.ml # 语义错误类型
├── IR/              # 中间表示 & 代码生成
│   ├── ANF.ml          # A-Normal Form IR 定义 + CPS block/label/jump
│   ├── Conversion.ml   # AST → ANF 转换 (CPS-based)
│   ├── ConvertMatch.ml # 模式匹配 → ANF branching 转换
│   └── WasmGen.ml      # ANF → WASM Text Format (.wat) 代码生成
└── doc/             # 文档与设计讨论
```

### Testing

```bash
# 运行所有测试
dune test
dune build @runtest

# 运行单个测试套件
dune build @runtest          # 全部测试
dune exec IR/test/ConversionTest.exe -- IR/test/*.dp  # 仅 ANF 转换测试
dune exec IR/test/WasmGenTest.exe -- IR/test/*.dp      # 仅 WAT 生成测试
```

### 编译示例

以下是三个完整的编译示例，展示从 Deeplang 源码到 ANF 中间表示、再到 WAT 的完整转换。

#### 示例 1：简单函数

**Deeplang 代码：**
```deeplang
fun main() -> I32 {
  return 0;
}
```

**ANF 中间表示：**
```
fun main() -> #1 =
  jump #1 (0)
```

`main` 函数编译为 CPS 风格的块，`jump #1 (0)` 表示跳转到返回标签 `#1` 并传递返回值 `0`。

**WASM S 表达式 (WAT)：**
```lisp
(module
  (memory (export "memory") 1)
  (global $heap_ptr (mut i32) (i32.const 1024))
  ;; bump_init, bump_alloc, bump_alloc_zero ...
  (func $main (result i32)
    i32.const 0
    br $l1
    (block $l1 (nop))))
```

`jump #1 (0)` 编译为 `i32.const 0` + `br $l1`：常量 `0` 压栈后跳转到函数出口标签。

---

#### 示例 2：Struct 构造

**Deeplang 代码：**
```deeplang
type Foo { a: (), b: I32 }

fun main() -> I32 {
  let x1: Foo = Foo { a: (), b: 114 };
  return 0;
}
```

**ANF 中间表示：**
```
fun main() -> #1 =
  $1 = mk((ANF.Struct "Foo"))(0, 114)
  jump #1 (0)
```

`mk((ANF.Struct "Foo"))(0, 114)` 在堆上分配 `Foo` 并用 `(0, 114)` 初始化。

**WASM S 表达式（main 函数）：**
```lisp
(func $main (result i32)
  i32.const 8           ;; Foo 大小 = 2 字段 × 4 字节
  call $bump_alloc      ;; heap_ptr += 8, 返回原指针
  i32.const 0           ;; 字段 a: unit
  i32.const 114         ;; 字段 b: I32
  ...
  i32.const 0
  br $l1
  (block $l1 (nop)))
```

结构体内存布局采用 bump allocator 模型：`$bump_alloc` 从堆上线性分配。

---

#### 示例 3：表达式与作用域

**Deeplang 代码：**
```deeplang
fun main() {
  let x = 1 + 2;
  let y = x + x * 3;
  let z = x - 3;
}
```

**ANF 中间表示：**
```
fun main() -> #1 =
  $1 = (+)(1, 2)         ;; 1 + 2  → $1 = 3
  $2 = (*)($1, 3)        ;; x * 3  → $2 = 9
  $3 = (+)($1, $2)       ;; x+x*3  → $3 = 12
  $4 = (-)($1, 3)        ;; x - 3  → $4 = 0
  jump #1 ()
```

所有嵌套表达式被展平（flatten），每个子表达式的结果绑定到临时变量（`$1`, `$2`, …）。

**WASM S 表达式（main 函数）：**
```lisp
(func $main (result i32)
  i32.const 1
  i32.const 2
  i32.add                ;; $1 = 1 + 2
  local.set $v1
  local.get $v1
  i32.const 3
  i32.mul                ;; $2 = $1 * 3
  local.set $v2
  local.get $v1
  local.get $v2
  i32.add                ;; $3 = $1 + $2
  local.set $v3
  local.get $v1
  i32.const 3
  i32.sub                ;; $4 = $1 - 3
  local.set $v4
  br $l1
  (block $l1 (nop)))
```

`(+)(1, 2)` 编译为 WASM `i32.add`，ANF 变量（`$1`）映射为 WASM 局部变量（`$v1`）。

---

**测试覆盖：**

| 测试 | 模块 | 覆盖范围 |
|------|------|---------|
| ParserTest | Syntax/test | 语法错误：表达式、模式、语句、类型声明等 50+ 错误用例 |
| WalkerTest | Semantics/test | 语义分析：表达式、函数、模式匹配、语句、类型声明 |
| ConversionTest | IR/test | AST→ANF 转换：基础类型、struct、ADT、控制流、match |
| WasmGenTest | IR/test | 端到端 WAT 生成：.dp → ANF → .wat |

### 生成文档

项目使用 [odoc](https://github.com/ocaml/odoc) 生成 OCaml 模块的 HTML 文档：

```bash
# 安装 odoc
opam install odoc -y

# 生成文档
make doc

# 或手动执行
dune build @doc-private
rm -rf doc/internal/*
cp -r _build/default/_doc/_html/* doc/internal/
```

生成的文档位于 `doc/internal/`，按模块组织：
- `doc/internal/Syntax/` — ParseTree, Parser, Lexer
- `doc/internal/Semantics/` — Table, Helper, Walker
- `doc/internal/IR/` — ANF, Conversion, WasmGen

可通过浏览器直接打开 `doc/internal/index.html` 浏览。

`doc/` 目录下还有设计文档与教程：

| 文档 | 内容 |
|------|------|
| [Deeplang-spec.md](doc/Deeplang-spec.md) | Deeplang 语言规范 |
| [simple-ANF.md](doc/simple-ANF.md) | ANF 变换教程，末尾连接至 [IR/ANF.ml](IR/ANF.ml) 的实际实现 |
| [simple-compiler.md](doc/simple-compiler.md) | 极简编译器教程，末尾连接至 Deeplang 完整流水线 |
| [walker.md](doc/walker.md) | Walker 模式教程，末尾连接至 [Semantics/Walker.ml](Semantics/Walker.ml) |
| [WebAssembly.md](doc/WebAssembly.md) | WASM 与 S 表达式简介 |
| [mm-impl-doc.md](doc/mm-impl-doc.md) | 内存管理（所有权/借用/引用计数）实现方案 |
| [deeplang_frontend_discuss.md](doc/deeplang_frontend_discuss.md) | 编译器前端设计讨论与符号表方案 |

### 开发指南

本项目使用 [dune](https://dune.build/) 构建系统。

添加新 OCaml 模块时：
1. 在对应目录创建 `.ml` 文件
2. 更新同目录的 `dune` 文件，将模块名加入 `(modules ...)` 列表
3. 如需跨库引用，在 `dune` 的 `(libraries ...)` 中添加依赖

详细的 dune 文档： [dune.readthedocs.io](https://dune.readthedocs.io/en/stable/overview.html)。

## 有趣的项目

We are learning TAPL and developing some interesting projects as follow:

- [untyped-lambda-calculus](http://mepy.net/untyped-lambda-calculus/) based on javascript
- [Dart-Lambda-Calculus](https://github.com/sorrowfulT-Rex/Dart-Lambda-Calculus) based on Dart

## License

MIT License

Copyright (c) 2026 Deeplang contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
