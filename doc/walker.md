# 函数式语言中的Walker

## 1. 递归与模式匹配与其不足
在这份小文档中，我们将使用一门简单的四则运算语言作为例子：
```
type expr =
    | Int of int
    | Add of expr * expr
    | Mul of expr * expr
```
上面的例子是一个非常典型的AST定义。
通过模式匹配和递归，我们可以实现对表达式的求值、显示等功能：
```
let rec eval expr =
    match expr with
    | Int i     -> i
    | Add(l, r) -> eval l + eval r
    | Mul(l, r) -> eval l * eval r

let rec to_string expr =
    match expr with
    | Int i     -> string_of_int i
    | Add(l, r) -> "(" ^ to_string l ^ " + " ^ to_string r ^ ")"
    | Mul(l, r) -> "(" ^ to_string l ^ " * " ^ to_string r ^ ")"
```
到此为止，一切都没什么问题。
下面，让我们考虑一些优化。
例如，我们可以消除形如`x + 0`、`x * 1`、`x * 0`的表达式：
```
let rec opt_plus_0 expr =
    match expr with
    | Int i -> Int i
    | Add(l, r) ->
        begin match opt_plus_0 l, opt_plus_0 r with
        | Int 0, x
        | x, Int 0 -> x
        | l, r     -> Add(l, r)
        end
    | Mul(l, r) ->
        Mul(opt_plus_0 l, opt_plus_0 r)

let rec opt_mul_1 expr =
    match expr with
    | Int i -> Int i
    | Add(l, r) -> Add(opt_mul_1 l, opt_mul_l r)
    | Mul(l, r) ->
        begin match opt_mul_1 l, opt_mul_1 r with
        | Int 1, x
        | x, Int 1 -> x
        | l, r     -> Mul(l, r)
```
上面的两个优化函数能够正常工作。
但当我们试图将它们组合起来时，就会产生两个问题：

- 无论以何种顺序先后执行，总需要至少遍历两次AST
- 假如先执行`opt_mul_1`，再执行`opt_plus_0`，
有可能`opt_plus_0`的化简过后又出现了形如`x * 1`的表达式，
使得优化被错过了。例如`2 * (0 + 1)`

此外，在这两个优化函数中，
我们关心的其实只有一种构造。
但当AST中有很多构造时，
对于每种无关构造我们都需要递归地优化子表达式，
这会产生大量的重复代码。

## 2. Walker to the rescue
上述问题的本质是：递归函数无法“拆开”变成单步的计算，
因而不能被灵活地组合。
因此，我们可以尝试把递归函数中“单步的计算”这一概念提取出来，
变成一个独立的数据结构。
这样，我们就能通过复合“单步的计算”，来复合递归函数了。
这一“单步的计算”的概念，就是walker。
为了表达对`expr`的单步的计算，我们需要提供**对每一种情况的处理方式**。
所以，我们可以把一个计算结果类型为`'a`的walker表示为：
```
type 'a expr_walker =
    { int : int -> 'a
    ; add : 'a -> 'a -> 'a
    ; mul : 'a -> 'a -> 'a }
```
这里，`add`、`mul`的参数的意义是：对子表达式递归地进行计算得到的结果。
由此，我们可以将`eval`的单步计算定义为：
```
let eval_walker : int expr_walker =
    { int = (fun i -> i)
    ; add = (fun l r -> l + r)
    ; mul = (fun l r -> l * r) }
```
现在，我们需要将单步计算转化为一个递归函数：
```
let rec run_walker : 'a expr_walker -> expr -> 'a = fun walker expr ->
    match expr with
    | Int i     -> walker.int i
    | Add(l, r) -> walker.add (run_walker walker l) (run_walker walker r)
    | Mul(l, r) -> walker.mul (run_walker walker l) (run_walker walker r)
```
于是我们得到了`eval`使用walker的等价表示：
```
let eval' : expr -> int = run_walker eval_walker
```
那么，上面提到的、递归函数的问题如何用walker解决呢？
首先，观察到优化函数的类型都是`expr -> expr`，
因此它们对应的walker类型应当是`expr expr_walker`。
由于这是一种十分常见的walker，它们有一个单独的名字`mapper`：
```
type expr_mapper = expr expr_walker
```
这个名字来源于“从一个表达式映射到另一个表达式”这一概念。
首先，让我们解决“不关心的构造产生大量无关代码”的问题。
对一个表达式进行映射时，无关的构造并不会被改动，
但是需要递归地对子表达式进行映射。
现在，`run_walker`已经帮我们完成了这一递归的过程。
那么，我们就只需要在这些情况什么都不做就可以了：
```
let trivial_mapper : expr_mapper =
    { int = (fun i -> Int i)
    ; add = (fun l r -> Add(l, r))
    ; mul = (fun l r -> Mul(l, r)) }
```
可以证明，对于任意的表达式`e`，有`run_walker trivial_mapper e = e`。
现在，当我们想要实现一些不trivial的mapper时，
**不关心的构造的处理方式和`trivial_mapper`是一致的**。
因此，我们可以直接复用`trivial_mapper`中的一部分！
基于这一观察，上述的两个优化函数可以用mapper表示为：
```
let opt_plus_0_mapper : expr_mapper =
    { trivial_mapper with
        add = (fun l r ->
            match l, r with
            | Int 0, x
            | x, Int 0 -> x
            | _        -> Add(l, r)) }

let opt_mul_1_mapper : expr_mapper =
    { trivial_mapper with
        mul = (fun l r ->
            match l, r with
            | Int 1, x
            | x, Int 1 -> x
            | _        -> Mul(l, r)) }
```
观察到，我们只需要定义我们关心的那些构造即可。
当AST中的构造数量极其多时，这可以节省极大量的代码。
下面，我们解决剩下的两个问题：组合性的问题。
想要组合两个用mapper的形式定义的递归函数，
我们只需要**将对应的mapper（单步计算）复合即可**。
在上面的例子里，`opt_plus_0_mapper`与`opt_mul_1_mapper`关心的构造是不同的，
所以我们只需要将它们各自关心的构造组合起来即可：
```
let opt_plus_0_mul_1_mapper : expr_mapper =
    { trivial_mapper with
        add = opt_plus_0_mapper.add;
        mul = opt_mul_1_mapper.mul }
```
我们可以验证`let opt_plus_0_mul_1 = run_walker opt_plus_0_mul_1_mapper`
的确能够同时完成这两种优化，而且没有上述的错过优化机会的问题。

最后，当我们要组合的mapper有重叠的case时，又应该怎么办呢？
我们可以用一个通用的mapper复合函数来完成这一工作：
```
let compose_mapper (m1 : expr_mapper) (m2 : expr_mapper) : expr_mapper =
    let run_m2_shallow expr =
        match expr with
        | Int i     -> m2.int i
        | Add(l, r) -> m2.add l r
        | Mul(l, r) -> m2.mul l r
    in
    { int = (fun i -> run_m2_shallow (m1.int i)
    ; add = (fun l r -> run_m2_shallow (m1.add l r))
    ; mul = (fun l r -> run_m2_shallow (m1.mul l r)) }
```
这里，`run_m2_shallow`做的事情与`run_walker`类似，
只不过它并不递归地处理子表达式：因为`run_walker`会帮我们完成这一点。
由于`m1`可能会使表达式的形状发生改变，
我们必须在`run_m2_shallow`中通过一次额外的模式匹配来应用`m2`。

## 总结
那么，什么时候应当使用walker/mapper来代替递归函数呢？
我们可以通过探究walker/mapper**不能**做什么来回答这一问题：

- 如果只有一个walker/递归函数要实现，那么此时使用walker没有任何意义。
- 如果有多个递归函数要实现，但我们不需要它们的组合，
或者它们之间有不能写入一次遍历的前后依赖关系，那么walker也无法带来什么好处。

由此，我们可以知道下列场景使用walker能够带来好处：

- 当我们有多个遍历操作，而且它们可以写入同一次遍历，
并且出于模块化考虑，它们应当被分开实现
- 当我们要对一个现有的遍历操作做一些修改，
得到一个新的遍历操作


## Deeplang 中的 Walker 实现

上述简单四则运算的例子展示了 walker/mapper 的核心思想。
在 Deeplang 编译器中，Walker 模式被广泛应用在语义分析阶段：

### 入口：walk_top

[Walker.ml](../Semantics/Walker.ml) 的入口是 `walk_top`，
它对顶层声明进行语义遍历：

```ocaml
val walk_top : context -> top_clause -> unit
```

`top_clause` 包括函数定义、类型声明、Interface 声明等。
`walk_top` 将每种声明分发到对应的处理函数。

### context：携带遍历状态

与简单例子中的 `'a expr_walker` 不同，Deeplang 的 Walker
需要携带丰富的状态信息。这些信息封装在 `context` 记录中：

```ocaml
(* Semantics/Walker.ml *)
type context = {
    table    : Table.table;       (* 符号表：变量、函数、类型 *)
    nametbl  : (string, ...) Hashtbl.t;  (* 名字解析表 *)
    scope    : string list;       (* 当前作用域栈 *)
    this     : typ;               (* 当前方法所属类型 *)
    rety     : typ;               (* 当前函数返回类型 *)
    checkloop : int;              (* 循环嵌套层数（0=不在循环中） *)
}
```

### 主要 walk 函数

```ocaml
val walk_expr  : context -> ParseTree.expr -> typ
val walk_stmt  : context -> ParseTree.stmt -> unit
val walk_pat   : context -> ParseTree.pattern -> (string * typ) list
val walk_func  : context -> ParseTree.func_decl -> unit
```

每个函数对 AST 节点进行自顶向下的遍历：
- `walk_expr` 对表达式进行类型检查并返回其类型
- `walk_stmt` 遍历语句体，维护作用域和循环上下文
- `walk_pat` 分析模式匹配，返回绑定的变量及其类型
- `walk_func` 检查函数签名并将其注册到符号表

### Interface 继承展开

Walker 中实现了 Interface 的 `extends` 机制。
当遇到 `interface I extends J { ... }` 时，
`walk_top` 会将父 interface `J` 的方法表复制到子 interface `I` 中，
并检测方法名冲突：

```ocaml
(* Semantics/Walker.ml - walk_top 中 InterfaceDecl 分支 *)
let copy_from_parent pname =
  match Hashtbl.find_opt table.typ pname with
  | Some (Intf_data { meth }) ->
      Hashtbl.iter (fun mname mdata ->
        if Hashtbl.mem fun_table mname then
          error_type (Error ("Method " ^ mname ^ " conflict from extended interface " ^ pname))
        else Hashtbl.add fun_table mname mdata) meth
  | _ -> error_type (Error ("Extended interface " ^ pname ^ " not found"))
in
List.iter copy_from_parent decl.intf_decl_extends;
```

### 错误报告

语义错误通过 `SemanticsError` 模块报告：

```ocaml
(* Semantics/SemanticsError.ml *)
type error =
    | Error of string
    | TypeError of typ * typ
    | UnboundVar of string
    | Redefinition of string
    ...
```

### 与简化 walker 的对比

| 特性 | 简化 walker | Deeplang Walker |
|------|------------|----------------|
| 模式 | mapper (记录类型) | 具名函数 |
| 状态 | 无（纯函数） | `context` 记录 |
| 遍历 | `run_walker` 递归 | 函数间相互递归调用 |
| 副作用 | 无 | 修改符号表、产生错误 |
| 复合 | `compose_mapper` | 不直接支持（通过 context 传递） |
| 类型 | 代数数据类型 | 完整的 Deeplang AST |
