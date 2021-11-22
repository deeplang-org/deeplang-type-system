# 用OCaml实现一个极简单编程语言

## AST
我们首先需要定义AST。
这里，我们考虑的语言是一门只有局部变量、布尔和整数的语言：
```
type expr =
    | Var  of string
    | Bool of bool
    | Int  of int
    | Add  of expr * expr
    (* 两个整数的相等判定 *)
    | IEq  of expr * expr
    (* If(cond, conseq, alter) = "if (cond) then { conseq } else { alter }" *)
    | If   of expr * expr * expr
    (* Let(name, rhs, body) = "let name = rhs in body" *)
    | Let  of string * expr * expr
```
虽然这门语言十分简单，但它依然覆盖了语言实现中的许多基本问题。
例如类型检查的问题：不应该允许两个布尔值相加。
作用域的问题：下面的代码：
```
(* let y = (let x = 1 in x) in x *)
Let("y", Let("x", Int 1, Var "x"), Var "x")
```
应当被拒绝，因为`x`在其作用域外被使用了。

## 在AST中加入额外信息：源码位置
编译器中一个很常见的需求是：为了输出足够好的错误信息，
我们需要知道AST中的某个节点对应源代码中的哪个位置。
为此，我们必须将位置信息放到每个AST节点中去。
虽然我们可以给AST中的每种构造都加上额外的位置数据，
但这样的解决方案并不理想。因为：

- 需要写大量重复的boilerplate代码
- 位置信息会污染每一处模式匹配的代码
- 无法不经模式匹配直接拿到一个AST节点的位置

事实上，有一种方法能够使我们系统化地给每个AST节点加上额外数据。
我们将AST的“形状”与“携带的数据”分为两个互相递归的类型来定义。
利用这种方法，上面的AST可以改写成：
```
type expr =
    (* AST的“形状” *)
    { shape : expr_shape
    (* 源码位置：行号+列号 *)
    ; loc   : (int * int) }

(* and表明两个类型是互相递归的 *)
and expr_shape =
    | Var  of string
    | Bool of bool
    | Int  of int
    (* 注意这里递归存储的是expr而非expr_shape
     * 因此每层AST都能附上额外信息 *)
    | Add  of expr * expr
    | IEq  of expr * expr
    | If   of expr * expr * expr
    | Let  of string * expr * expr
```
上面提到的三个问题都被完美地解决了：

- 额外数据只需要在`expr`的定义里声明一次
- 模式匹配时不需要关心位置信息
- 不需要对形状模式匹配，直接`x.loc`即可获得额外数据

我们甚至可以通过使`expr`和`expr_shape`变成一个带参数的类型，
来定义出**可以存放任意额外数据的一个AST家族**：
```
type 'a expr =
    (* AST的“形状” *)
    { shape : 'a expr_shape
    (* 源码位置：行号+列号 *)
    ; loc   : (int * int)
    (* 由类型参数'a决定的其他额外数据 *)
    ; extra : 'a }

and 'a expr_shape =
    (* 把所有expr换成'a expr即可 *)
    ...
```
当我们不想附加额外数据时，在`extra`中填入占位的`unit`即可：
```
type raw_expr = unit expr
```

## 类型检查
在我们考虑的语言中，只有两种简单的类型：
```
type typ = Ty_Int | Ty_Bool
```
在类型检查时，我们必须知道每个变量的类型是什么。
所以，我们需要一个变量到其类型的映射：
```
module VarMap = Map.Make(String)

type env = typ VarMap.t
```
为了将类型信息传递给后续的编译管线，
我们利用AST附加额外数据的能力，
在类型检查后给每一个AST节点附上它的类型。
因此，类型检查函数的签名就可以写成：
```
val type_check : env -> raw_expr -> typ expr
```
类型检查可以通过一个递归函数来完成：
```
(* 表示类型错误的异常，包含一条错误信息和一个位置 *)
exception Type_Error of string * (int * int)

let rec type_check env expr =
    (* 注意这里需要对shape进行模式匹配 *)
    match expr.shape with
    | Var name ->
        (* 在env查找变量对应的类型。
         * 这意味着只要我们正确地维护了env，
         * 类型检查的同时也完成了作用域检查。*)
        begin match VarMap.find_opt name env with
        | Some typ ->
            (* 由于type_check返回一颗新的、带类型信息的AST，
             * 我们需要构建新的AST节点，并将类型信息放在extra中 *)
            { shape = Var name
            ; loc   = expr.loc
            ; extra = typ }
        | None ->
            raise(Type_Error("unbound variable: " ^ name, expr.loc))
        end
    | Bool value ->
        { shape = Bool value
        ; loc   = expr.loc
        ; extra = Ty_Bool }
    | Int value ->
        { shape = Int value
        ; loc   = expr.loc
        ; extra = Ty_Int }
    (* 第一个递归的构造 *)
    | Add(lhs, rhs) ->
        (* 我们使用type_check直接递归检查子表达式的类型。
         * 由于Add没有引入新的变量，不需要改动env *)
        let lhs' = type_check env lhs in
        let rhs' = type_check env rhs in
        (* lhs'和rhs'都是附带了类型信息的AST。
         * 所以我们可以直接拿出它们的类型进行检查 *)
        begin match lhs'.extra, rhs'.extra with
        | Ty_Int, Ty_Int ->
            { shape = Add(lhs', rhs')
            ; loc   = expr.loc
            ; extra = Ty_Int }
        | _ ->
            raise(Type_Error("type mismatch: only integers can be added", expr.loc))
        end
    | IEq(lhs, rhs) ->
        let lhs' = type_check env lhs in
        let rhs' = type_check env rhs in
        begin match lhs'.extra, rhs'.extra with
        | Ty_Int, Ty_Int ->
            { shape = IEq(lhs', rhs')
            ; loc   = expr.loc
            ; extra = Ty_Bool }
        | _ ->
            raise(Type_Error("type mismatch: only integers can be compared", expr.loc))
        end
    | If(cond, conseq, alter) ->
        let cond' = type_check env cond in
        let conseq' = type_check env conseq' in
        let alter'  = type_check env alter'  in
        
        if cond'.extra <> Ty_Bool then
            raise(Type_Error("type mismatch: 'if' only accepts boolean as condition", expr.loc));

        if conseq'.extra <> alter'.extra then
            raise(Type_Error("type mismatch: two branches of 'if' must have the same type", expr.loc));

        { shape = If(cond', conseq', alter')
        ; loc   = expr.loc
        ; extra = conseq'.extra }
    (* 涉及到作用域的构造 *)
    | Let(name, rhs, body) ->
        (* 按照作用域规则，rhs不在name的作用域中。
         * 所以rhs被类型检查的上下文就是env *)
        let rhs' = type_check env rhs in
        (* body应当在name被定义的情况下检查。
         * 所以我们传给body的上下文中多了name。
         * 如果env中已经有同名变量，它会被覆盖掉。
         * 这也是符合作用域规则的。*)
        (* 这里十分关键的一点是：env是一个纯函数式/可持久化的数据结构。
         * 所以VarMap.add会创建出新的映射，而不会修改原来的map。
         * 所以只有body会受到这次VarMap.add的影响，其他表达式都不会。
         * 这就使得作用域规则通过env得到了正确的维护。*)
        let body' = type_check (VarMap.add name rhs'.extra env) body in
        { shape = Let(name, rhs', body')
        ; loc   = expr.loc
        ; extra = body'.extra }
```
