# ANF变换的简单介绍

## ANF是什么
在大部分语言的语法中，允许各种嵌套的表达式。
例如`2 * (3 + 4)`、`f(x, g(y, z))`等。
但是，嵌套的表达式会对编译器的一些操作带来不便：

- 嵌套表达式的求值顺序是隐式的，而不是显式的
- 如果语言区分了表达式和语句，
那么一些需要插入语句的操作（例如智能指针的复制和销毁）
不方便

为了解决这些问题，可以引入一种中间表示。
但与此同时，我们希望着这种中间表示是贴近语言语法的、易于理解的，
这会使得编译器中的各种操作更简单。
而ANF（Administrative Normal Form，或者A-Normal Form）
就是这样一种中间表示。
在ANF中，**不存在嵌套的表达式**，
一切子表达式必须被提取出来，用一个变量表达。
例如，`2 * (3 + 4)`被翻译成ANF后会变成：

    let x = 3 + 4 in
    2 * x

`f(x, g(y), h(z))`被翻译成ANF后会变成：

    let x1 = g(y) in
    let x2 = h(z) in
    f(x, x1, x2)

由于ANF中没有复合表达式，对它进行各种操作都会变得更简单。
例如，如果上面的例子中，`y`是一个智能指针，
在`g(y)`使用它后需要插入一个`drop(y)`的操作来销毁它。
在`f(x, g(y), h(z))`中，直接插入`drop(y)`是不可能的。
因此我们只能把`drop(y)`插入到整个`f(x, g(y), h(z))`后。
但如果`y`占用了很多内存，而`h`或`f`需要很长时间来运行的话，
`y`占用的内存在`h`和`f`运行期间就可能处于“可以释放但未被释放”的泄漏状态。
但是，在ANF下就没有这个问题。我们可以直接把程序重写成：

    let x1 = g(y) in
    drop(y);
    let x2 = h(z) in
    f(x, x1, x2)

所以，ANF这一形式可以简化很多编译器操作。
此外，由于在ANF中没有嵌套表达式，
所以对于源程序中的每一个表达式，
它要么是一个字面量，要么就会在ANF中有一个对应的变量与之匹配。
这意味着，如果我们需要给源程序中的每个子表达式附上额外数据（例如类型信息），
在ANF中只需要给每个（ANF中的）变量附上额外数据即可。


## 一门简单的语言及其对应的ANF
接下来，我们通过一个很简单的语言来演示ANF变换。
我们的源语言的AST是：

    type expr =
        | Int of int
        | Var of string
        | Add of expr * expr
        | IfZ of expr * expr * expr
        | Let of string * expr * expr

其中：

- `Int i`是整数字面量`i`
- `Var v`是名字为`v`的变量
- `IfZ(i, a, b)`相当于`if (i = 0) then a else b`
- `Let(x, rhs, body)`相当于`let x = rhs in body`，
是局部变量声明

这个无比简单的AST依然能够反映很多问题：

- `Add`的存在引入了复合表达式
- `Let`引入了变量重名问题和作用域
- `IfZ`引入了控制流分叉。事实上，如何处理分支是ANF变换中的一个重要问题

为了方便查看AST的内容，我们可以定义如下的函数。
为了输出的美观它使用了OCaml标准库中用于pretty printing的模块`Format`。
读者不需要了解`Format`的使用方法，
但在运行这篇文档中的代码时可以使用`print_expr e`来查看`e : expr`。

    let rec fprint_expr level fmt expr =
        let open Format in
        match level, expr with
        | _, Int i -> Format.fprintf fmt "%d" i
        | _, Var v -> Format.fprintf fmt "%s" v
        | 0, _     -> Format.fprintf fmt "(%a)" (fprint_expr 1) expr 
        | _, Add(a, b) ->
            Format.fprintf fmt "@[<hov2>%a@ + %a@]"
                (fprint_expr 0) a
                (fprint_expr 0) b
        | _, IfZ(c, a, b) ->
            Format.fprintf fmt "@[<hv2>if %a@ then %a@ else %a@]"
                (fprint_expr 0) c
                (fprint_expr 2) a
                (fprint_expr 2) b
        | 1, _ ->
            Format.fprintf fmt "(%a)" (fprint_expr 2) expr 
        | _, Let(var, rhs, body) ->
            Format.fprintf fmt "@[<hv>let %s = %a in@ %a@]"
                var
                (fprint_expr 1) rhs
                (fprint_expr 2) body


    let fprint_expr = fprint_expr 2
    let print_expr expr = Format.printf "%a" fprint_expr expr

接下来，我们希望从`expr`中消除所有嵌套表达式。
这包括嵌套的`Add`和`Let`
（例如`let x = (let y = 1 in y) in x`中的`let y = 1 in y`就引入了一个局部作用域）。
首先，我们需要定义简化后的ANF形式。
为此，我们需要定义“最小”的原子表达式`anf_value`：

    type anf_var = int
    type anf_value = ANF_Int of int | ANF_Var of anf_var

`anf_value`是无论如何都无法继续化简的表达式。
注意到我们在ANF中使用`type anf_var = int`作为变量类型。
这能使编译器后续操作中查找变量名更高效，
同时通过适当的新变量名产生机制可以使ANF中不再有变量重名问题。

现在，我们可以定义ANF中的表达式和整个程序：

    type anf_expr =
        | ANF_Value of anf_value
        | ANF_Add   of anf_value * anf_value
        | ANF_IfZ   of anf_value * anf_program * anf_program

    and anf_program =
        { anf_decls  : (anf_var * anf_expr) list
        ; anf_result : anf_value }

其中：

- `ANF_Value`允许我们直接把一个`anf_value`当成`anf_expr`
- `ANF_Add`是`Add`的对应，注意到它的两个参数都是`anf_value`，
所以嵌套的表达式在语法层面被禁止了
- `ANF_IfZ`对应`IfZ`，注意到它的两个分支是完整的`anf_program`。
后面将详细解释这一点
- `anf_program`对应源程序中一个完整的`expr`。
它包含一系列变量定义（`anf_decls`）和一个最终结果（`anf_result`）

在比较正统的ANF变换中，`IfZ`不应该成为一个`anf_expr`，
而应该作为`anf_program`的一种形式。
因为`IfZ`作为`anf_expr`事实上允许了任意`anf_program`嵌套在`anf_expr`中。
但为了禁止`IfZ`的嵌套，要么生成的ANF的大小会指数爆炸，
要么需要用一些更复杂的机制来共享“使用`IfZ`的结果”的“continuation”。
这里为了简单起见，允许`IfZ`作为`anf_expr`。

注意到，每个`anf_program`其实依然是合法的`expr`。
我们可以轻松地写出一个`anf_to_expr`的函数：

    let anf_val_to_expr anf_var =
        match anf_var with
        | ANF_Int i -> Int i
        | ANF_Var v -> Var("x" ^ string_of_int v)

    let rec anf_expr_to_expr anf_expr =
        match anf_expr with
        | ANF_Value v      -> anf_val_to_expr v
        | ANF_Add(a, b)    -> Add( anf_val_to_expr a
                                 , anf_val_to_expr b )
        | ANF_IfZ(c, a, b) -> IfZ( anf_val_to_expr c
                                 , anf_prog_to_expr a
                                 , anf_prog_to_expr b )

    and anf_prog_to_expr anf_prog =
        let rec add_decls decls result =
            match decls with
            | [] -> result
            | (anf_var, rhs) :: decls' ->
                Let( "x" ^ string_of_int anf_var
                   , anf_expr_to_expr rhs
                   , add_decls decls' result )
        in
        add_decls anf_prog.anf_decls
            (anf_val_to_expr anf_prog.anf_result)

利用`anf_prog_to_expr`，我们可以免费得到`print_anf_prog`来查看ANF的表达式
（这里为了方便使用做了一些与`Format`互动的支持，读者可以无视这两个辅助函数）：

    let fprint_anf_prog fmt anf_prog =
        fprint_expr fmt (anf_prog_to_expr anf_prog)

    let print_anf_prog anf_prog =
        print_expr (anf_prog_to_expr anf_prog)

这说明，ANF是一种非常贴近源语言的中间表示。
在ANF上实现编译器的各种操作和在源语言上实现是高度相似的，
只是少了一些需要考虑的情况（嵌套表达式）。
这正是ANF的一大优势，也是将它和其他流行的编译器中间表示区分开的重要特征：

- 函数式语言编译器中的CPS变换能够暴露出比ANF更多的信息和优化空间。
但CPS和源程序之间的差异很大，更难阅读、理解
（事实上，ANF最初就是作为“更友好的CPS”被提出的）
- LLVM以及其他许多编译器中采用的SSA形式，同样比ANF能够暴露出更多数据流信息。
但它的`phi`节点是源程序中不存在、不那么容易理解的构造。
此外，SSA中控制流是用更底层的基本块和`goto`实现的，这也使它比ANF更难理解。


## 一个简单的ANF变换
要把ANF作为编译器的中间表示，就需要把源程序转换为ANF，
也就是需要一个`expr_to_anf : expr -> anf_prog`的函数。
这一过程就称为ANF变换。

在写出通用的`expr_to_anf`之前，
不妨先看几个具体的例子。
考虑文档开头的例子`2 + (3 + 4)`，它应当被转化为：

    let x1 = 3 + 4 in
    let x2 = 2 + x1 in
    x2

接下来，考虑一个包含嵌套的`Let`和变量重名的例子
`let x = (let x = 1 in x) in x`。它应当被转化为：

    let x1 = 1 in // 对应括号中的那个x
    let x2 = x1 in // 对应括号外的那个x
    x2

注意到重名的两个`x`在ANF后变成了不同的ANF变量`x1`、`x2`。
因此，ANF变换中还能够免费把作用域解析的问题解决。
最后，考察一个带有`IfZ`的例子`1 + (ifZero 0 then 1 else 2)`。
它应当被转化为：

    let x1 = if (0 = 0) then 1 else 2 in
    let x2 = 1 + x1 in
    x2

下面，我们开始定义通用的`expr_to_anf`。
首先，我们需要解决`expr`中的变量如何对应到`anf_var`。
为此，我们可以维护一个map：

    module SMap = Map.Make(String)

接下来，我们还需要不断产生新的、“新鲜”（fresh）的`anf_var`：

    let anf_var_seed = ref 0
    let gen_anf_var () =
        incr anf_var_seed;
        !anf_var_seed

尽管整个`expr`应当被映射到一个`anf_prog`，
它之中的子表达式应当被映射到`anf_val`，以消除嵌套的表达式。
为此，我们需要生成一系列的ANF变量声明，
以把子表达式变成ANF变量。
所以，我们需要先定义如下的函数：

    expr_to_anf : anf_var SMap.t
                -> (anf_var * anf_expr) list
                -> expr
                -> anf_val * (anf_var * anf_expr) list

其中`anf_var SMap.t`的参数是从`expr`变量到ANF变量的映射。
`(anf_var * anf_expr) list`的参数是已经生成的ANF变量声明。
`expr`的参数是待转换的表达式，
而返回值是`expr`对应的`anf_val`和加入了必要的新声明的声明列表。
由于这个算法的特性，两个声明列表都是倒序的。
`expr_to_anf`的实现如下：

    let rec expr_to_anf env decls expr =
        match expr with
        | Int i -> ( ANF_Int i, decls )
        | Var v ->
            (* 在env中查找expr变量对应的ANF变量 *)
            begin match SMap.find_opt v env with
            | Some avar -> ( ANF_Var avar, decls )
            | None      -> failwith("unbound variable " ^ v)
            end
        | Add(a, b) ->
            (* 先计算出a和b的结果，然后算出a + b，保存在一个临时变量中 *)
            let a_val, decls'  = expr_to_anf env decls  a in
            let b_val, decls'' = expr_to_anf env decls' b in
            let tmp_var = gen_anf_var () in
            ( ANF_Var tmp_var, (tmp_var, ANF_Add(a_val, b_val)) :: decls'' )
        | IfZ(c, a, b) ->
            (* 先计算出c的结果，执行a或b，然后把结果保存在一个临时变量中 *)
            let c_val, decls' = expr_to_anf env decls c in
            let a_prog = expr_to_anf_prog env a in
            let b_prog = expr_to_anf_prog env b in
            let tmp_var = gen_anf_var () in
            ( ANF_Var tmp_var, (tmp_var, ANF_IfZ(c_val, a_prog, b_prog)) :: decls' )
        | Let(name, rhs, body) ->
            (* 给expr变量分配一个新的ANF变量，把rhs的结果绑定到它，
             * 然后计算body，在env中存储name对应到anf_var的事实 *)
            let rhs_val , decls'  = expr_to_anf env decls rhs in
            let anf_var = gen_anf_var () in
            let body_decls = (anf_var, ANF_Value rhs_val) :: decls' in
            expr_to_anf (SMap.add name anf_var env) body_decls body

    and expr_to_anf_prog env expr =
        let result_val, decls = expr_to_anf env [] expr in
        { anf_decls  = List.rev decls
        ; anf_result = result_val }

现在，我们可以通过`expr_to_anf_prog`来验证之前的几个例子：

    let _ = Format.printf "@[<v>"

    let test1 = Add(Int 2, Add(Int 3, Int 4))
    let _ =
        anf_var_seed := 0;
        Format.printf "%a@ =>@ %a@ @ "
            fprint_expr test1
            fprint_anf_prog (expr_to_anf_prog SMap.empty test1)

    let test2 = Let("x", Let("x", Int 1, Var "x"), Var "x")
    let _ =
        anf_var_seed := 0;
        Format.printf "%a@ =>@ %a@ @ "
            fprint_expr test2
            fprint_anf_prog (expr_to_anf_prog SMap.empty test2)

    let test3 = Add(Int 1, IfZ(Int 0, Int 1, Int 2))
    let _ =
        anf_var_seed := 0;
        Format.printf "%a@ =>@ %a@ @ "
            fprint_expr test3
            fprint_anf_prog (expr_to_anf_prog SMap.empty test3)
        
    let _ = Format.printf "@]"

只要将上面的所有代码片段放到同一个OCaml文件（例如`simple-ANF.ml`）中，
并运行`ocaml <文件名>`，就可以看到预期之中的输出：

    2 + (3 + 4)
    =>
    let x1 = 3 + 4 in let x2 = 2 + x1 in x2

    let x = (let x = 1 in x) in x
    =>
    let x1 = 1 in let x2 = x1 in x2

    1 + (if 0 then 1 else 2)
    =>
    let x1 = if 0 then 1 else 2 in let x2 = 1 + x1 in x2


## ANF变换前后应该做些什么？
如果要把ANF作为编译器的中间表示，一个必须要考虑的问题是：
哪些事应该放在ANF变换前、在原始的parse tree上完成；
哪些事应该放在ANF变换后、在ANF形式上完成。
在这里，我尝试进行一个不完整的总结：

- 作用域解析可以在ANF变换的途中直接完成，因此没必要在ANF变换前后单独再做
- 类型检查即可以在ANF前、也可以在ANF后做。因为ANF和源语言是高度相似的
- 一些需要数据流、控制流信息的操作，例如一些program transformation和优化、
以及borrow checker，更适合在ANF后做，因为ANF的结构更简单
- 语法糖的翻译一般会在ANF前完成，减少ANF形式中的噪音
- 诸如类型定义、interface或trait的解析等，可以在ANF前也可以在ANF后完成。
