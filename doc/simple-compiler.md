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
        let conseq' = type_check env conseq in
        let alter'  = type_check env alter  in
        
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

## 代码生成
下面我们尝试把这门语言编译成字节码。
我们的目标语言是一门简单的、只有整数的栈式虚拟机：
```
type instruction =
   (* 将一个整数字面量推到栈顶 *)
   | I_Push of int
   (* 从栈顶弹出两个元素，将它们的和推回栈顶 *)
   | I_Add
   (* 从栈顶弹出两个元素，
    * 如果它们相等，将1推到栈顶。
    * 否则将0推到栈顶 *)
   | I_Eq
   (* 弹出栈顶的元素，把它作为地址。
    * 读取该地址中的元素，并将它推到栈顶 *)
   | I_Load
   (* 弹出栈顶的元素，把它作为地址。
    * 再从栈中弹出第二个元素，存储到该地址中*)
   | I_Store
   (* 一个字节码中用于跳转的标签，
    * 用一个整数作为标识符 *)
   | I_Label of int
   (* 无条件地跳转到给定的标签 *)
   | I_Jmp of int
   (* 弹出栈顶的元素，如果它为0,
    * 跳转到给定的标签 *)
   | I_JmpIfZero of int
```
为了编译我们的语言，
我们需要给每个变量分配一个地址。
因此，生成目标代码的过程，我们需要一个变量名字到地址的映射：
```
type env = int VarMap.t
```
此外，我们还需要生成新的地址和标签。
这里，简单起见，我们直接使用可变状态来实现这两项功能
（但这意味着编译两段不同的程序时需要手动重设状态）：
```
let current_loc = ref (-1)
let gen_loc () =
    current_loc := 1 + !current_loc;
    !current_loc
    
let current_label = ref (-1)
let gen_label () =
    current_label := 1 + !current_label;
    !current_label
```
现在，编译的目的就是生成一串虚拟机指令。
所以我们可以写出编译函数的类型签名：
```
val compile : env -> typ expr -> instruction list
```
这里我们使用了带类型信息的AST作为输入。
在我们考虑的这门语言中，类型信息对编译没有太大帮助。
但对于更复杂的语言来说，类型信息对生成高效的代码是非常重要的。
`compile`函数生成一段指令。
在一个栈`s`上执行这段指令后，
栈应当变成`s`加上被编译的表达式计算的结果。
也就是说，执行这段指令只会往栈上添加一个新元素，
而且原来的栈中`s`中的元素不会被弹出。
最后，为了方便我们向指令序列中添加新的指令，
我们要求`compile`返回的指令序列是**倒序的**。
也就是说，链表的头部是最后的指令，尾部是最初的指令。

现在，我们可以写出`compile`函数：
```
let rec compile env expr =
    match expr.shape with
    (* 将name的值放到栈顶 *)
    | Var name ->
        (* 由于类型检查已经完成了作用域检查，
         * 我们可以假设这里的查找必定成功 *)
        let loc = VarMap.find name env in
        (* 记住这串指令要从右往左读 *)
        [ I_Load; I_Push loc ]
    | Bool true ->
        [ I_Push 1 ]
    | Bool false ->
        [ I_Push 0 ]
    | Int i ->
        [ I_Push i ]
    | Add(lhs, rhs) ->
        let lhs_code = compile env lhs in
        let rhs_code = compile env rhs in
        (* 从右往左，在栈s上先执行lhs的代码，栈变为s, lhs_result。
         * 接下来执行rhs的代码，栈变为s, lhs_result, rhs_result。
         * 最后用Add指令计算它们的和 *)
        I_Add :: (rhs_code @ lhs_code)
    | IEq(lhs, rhs) ->
        let lhs_code = compile env lhs in
        let rhs_code = compile env rhs in
        I_Eq :: (rhs_code @ lhs_code)
    | If(cond, conseq, alter) ->
        let cond_code = compile env cond in
        let conseq_code = compile env conseq in
        let alter_code  = compile env alter  in
        let label_alter  = gen_label () in
        let label_if_end = gen_label () in
        (* 这串指令的结构是：
         *   <cond_code> // 栈顶变成条件的结果
         *   if pop() == 0 then goto ALTER;
         *   <then_code>
         *   goto END
         * ALTER:
         *   <alter_code>
         * END: *)
        I_Label label_if_end
        :: alter_code
        @  I_Label label_alter
        :: I_Jmp label_if_end
        :: conseq_code
        @  I_JmpIfZero label_alter
        :: cond_code
    | Let(name, rhs, body) ->
        let rhs_code = compile env rhs in
        (* 根据作用域，编译body时应当给name分配一个地址 *)
        let loc = gen_loc () in
        let body_code = compile (VarMap.add name loc env) body in
        (* <rhs_code> // 栈是s, rhs_result
         * push loc   // 栈是s, rhs_result, loc
         * store      // memory[loc] := rhs_result, 栈变成s
         * <body_code> *)
        body_code
        @ I_Store
        :: I_Push loc
        :: rhs_code
```

## 用一个简单的虚拟机来测试代码
最后，通过实现一个简单的虚拟机，我们可以对上述函数进行测试
（这个虚拟机只追求语义的正确性，没有对性能做任何优化）：
```
module IntMap = Map.Make(Int)

let run_instructions instrs =
    let store = ref IntMap.empty in
    let program = Array.of_list instrs in

    let label_map = snd @@ Array.fold_left
            (fun (index, label_map) instruction ->
                        match instruction with
                        | I_Label label ->
                            (index + 1, IntMap.add label index label_map)
                        | _ ->
                            (index + 1, label_map))
            (0, IntMap.empty) program
    in

    let rec run stack index =
        match program.(index) with
        | exception _ ->
            stack
        | I_Push i ->
            run (i :: stack) (index + 1)
        | I_Add ->
            begin match stack with
            | v1 :: v2 :: stack' ->
                run ((v2 + v1) :: stack') (index + 1)
            | _ ->
                failwith "Runtime Error!"
            end
        | I_Eq ->
            begin match stack with
            | v1 :: v2 :: stack' ->
                run ((if v1 = v2 then 1 else 0) :: stack') (index + 1)
            | _ ->
                failwith "Runtime Error!"
            end
        | I_Load ->
            begin match stack with
            | loc :: stack' ->
                run (IntMap.find loc !store :: stack') (index + 1)
            | _ ->
                failwith "Runtime Error!"
            end
        | I_Store ->
            begin match stack with
            | loc :: value :: stack' ->
                store := IntMap.add loc value !store;
                run stack' (index + 1)
            | _ ->
                failwith "Runtime Error!"
            end
        | I_Label label ->
            run stack (index + 1)
        | I_Jmp label ->
            run stack (IntMap.find label label_map)
        | I_JmpIfZero label ->
            begin match stack with
            | 0 :: stack' ->
                run stack' (IntMap.find label label_map)
            | _ :: stack' ->
                run stack' (index + 1)
            | _ ->
                failwith "Runtime Error!"
            end
    in
    run [] 0
```
现在，我们已经有了一门简单语言除了parser以外的所有环节，
我们可以将之前定义的各个函数串联起来了，
形成一条完整的编译流水线了：
```
let process expr =
    expr
    |> type_check VarMap.empty
    |> compile VarMap.empty
    (* compile产生的指令序列是倒序的 *)
    |> List.rev
    |> run_instructions
```
我们也可以用一些测试表达式来测试这条编译流水线：
```
(* 由于没有parser，没有源码位置可以使用 *)
let mkExpr shape =
    { shape
    ; loc = (0, 0)
    ; extra = () }

(* let y = (let x = 1 in x)
 * in x *)
let test1 =
    mkExpr @@ Let(
        "y",
        mkExpr @@ Let("x", mkExpr @@ Int 1, mkExpr @@ Var "x"),
        mkExpr @@ Var "x"
    )
(* process test1
 * --> exception Failure("unbound variable x", (0, 0)) *)

(* let x = 1 in
 * let y = 2 in
 * if (x = y)
 * then 10
 * else (x + y) *)
let test2 =
    mkExpr @@ Let(
        "x", mkExpr @@ Int 1,
        mkExpr @@ Let(
            "y", mkExpr @@ Int 2,
            mkExpr @@ If(
                mkExpr @@ IEq(mkExpr @@ Var "x", mkExpr @@ Var "y"),
                mkExpr @@ Int 10,
                mkExpr @@ Add(mkExpr @@ Var "x", mkExpr @@ Var "y")
            )
        )
    )
(* process test2
 * --> [3]
 * 如果把x的值换成2，那么
 * process test2
 * --> [10] *)
```
