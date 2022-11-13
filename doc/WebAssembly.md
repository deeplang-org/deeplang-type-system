# WebAssembly 简单介绍

## WASM概述

WebAssembly，简称wasm，是基于栈式虚拟机的二进制指令集，可以作为编程语言的编译目标，能够部署在web客户端和服务端的应用中。

wasm规定了一套运行在虚拟机上的虚拟指令集，最初用于web中的汇编提高性能，现也可作为编译器的编译目标部署。

`WebAssembly` 于 `2019` 年 `12` 月 `5` 日成为万维网联盟（`W3C`）的推荐标准，与 `HTML`，`CSS` 和 `JavaScript` 一起成为 `Web` 的第四种语言。

对编译器来说，我们可以先生成wasm的S表达式，然后通过S表达式转换为相应的wasm机器码。

## S表达式

S表达式是指一种以人类可读的文本形式表达半结构化数据的约定。在S表达式中，每条语句都是先执行最里边括号的表达式然后依次展开。

S-表达式可以是如数字这样的单个对象，包括特殊原子nil和t在内的LISP 原子，或写作 (x . y)的cons pair。更长的列表则由嵌套的cons pair组成，例如(1 . (2 . (3 . nil)))

这里给出一段斐波那契数列求解的C语言代码与S表达式的代码

~~~c
#include <emscripten.h>
extern "C" {
  EMSCRIPTEN_KEEPALIVE 
  int fibonacci(int n) {
    if(n < 2) {
      return 1;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
  }
}
~~~



~~~Lisp
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func))
  (type (;3;) (func (param i32)))
  (func (;0;) (type 2)
    nop)
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32 i32)
    i32.const 1
    local.set 1
    local.get 0
    i32.const 2
    i32.ge_s
    if (result i32)  ;; label = @1
      i32.const 0
      local.set 1
      loop  ;; label = @2
        local.get 0
        i32.const -1
        i32.add
        call 1
        local.get 1
        i32.add
        local.set 1
        local.get 0
        i32.const 3
        i32.gt_s
        local.set 2
        local.get 0
        i32.const -2
        i32.add
        local.set 0
        local.get 2
        br_if 0 (;@2;)
      end
      local.get 1
      i32.const 1
      i32.add
    else
      i32.const 1
    end)
  (func (;2;) (type 0) (result i32)
    global.get 0)
  (func (;3;) (type 3) (param i32)
    local.get 0
    global.set 0)
  (func (;4;) (type 1) (param i32) (result i32)
    global.get 0
    local.get 0
    i32.sub
    i32.const -16
    i32.and
    local.tee 0
    global.set 0
    local.get 0)
  (func (;5;) (type 0) (result i32)
    i32.const 1024)
  (table (;0;) 2 2 funcref)
  (memory (;0;) 256 256)
  (global (;0;) (mut i32) (i32.const 5243920))
  (global (;1;) i32 (i32.const 1028))
  (export "memory" (memory 0))
  (export "__indirect_function_table" (table 0))
  (export "fibonacci" (func 1))
  (export "_initialize" (func 0))
  (export "__errno_location" (func 5))
  (export "stackSave" (func 2))
  (export "stackRestore" (func 3))
  (export "stackAlloc" (func 4))
  (export "__data_end" (global 1))
  (elem (;0;) (i32.const 1) func 0))
~~~



## WASM应用

在编译器后端开发的过程中，往往需要为每一种机器架构指令集都开发一个相应的后端，且各个操作系统间不能够兼容。

但如果有了wasm之后，我们就可以将`deeplang`译成 `wasm`，然后直接分发到各个平台上。

*Any application that can be compiled to WebAssembly, will be compiled to WebAssembly eventually.*