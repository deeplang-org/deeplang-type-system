
type variable    = int
type block_label = int

type func_name = Syntax.ParseTree.func_name
type adt_label = Syntax.ParseTree.adt_label
type typ_name  = Syntax.ParseTree.typ_name
type intf_name = Syntax.ParseTree.intf_name

type unary_op  = Syntax.ParseTree.unary_op
type binary_op = Syntax.ParseTree.binary_op

type span = Syntax.SyntaxError.src_span


type mutability = Imm | Mut
type permission = Owner | Borrow of mutability



(** [data_kind] is used to classify different kinds of aggregate data.
    In the ANF IR, different kinds of aggregate data share a uniform representation. *)
type data_kind =
    | Tuple  of int
    | Struct of typ_name
    | ADT    of typ_name * adt_label
    | Impl   of intf_name (** method dict/vtable of interface *)



(** [lvalue] is a value that may appear at the left hand side of an assignment.
    So every [lvalue] corresponds to a memory address.
    Some examples: [point.x], [*ptr] *)
type lvalue =
    { var  : variable
    ; path : path }

and path = path_node list

(** [path_node] is used to select a part of an existing [lvalue]:
    {ul
        {li [Field k] selects the [k]-th field from an aggregate,
        i.e. a tuple, a struct, an ADT or a method dictionary}
        {li [Deref] selects the address a pointer points to}
    } *)
and path_node =
    | Field of int
    | Deref


(** A [value] in the ANF IR is something immediately available without needing
    any computation. *)
type value =
    | Var   of variable
    | Int   of int
    | Float of float
    | Fun   of func_name


type expr =
    | Val    of value
    | Move   of lvalue
    | Copy   of lvalue
    | Borrow of mutability * lvalue
    | App    of value * value list
    | UnOp   of unary_op  * value
    | BinOp  of binary_op * value * value
    | MkData of data_kind * value list
    | TagOf  of value

type statement =
    | Decl     of variable * expr
    | Assign   of lvalue * value
    | EndScope of variable list


(** {ul
        {li [Return(span, expr)] returns the result of [expr] as the result of the whole program.
            If [expr] is a function application, then this represents a tail-call}
        {li [Jump(span, label, args)] jumps to the block with name [label] with [args]}
        {li [Stmt(span, stmt, body)] first executes [stmt] and then executes [body].
            [span] is the source location of [stmt]}
        {li [Branch br] is a unified construction for all control flow branching structures,
            e.g. if/else and pattern matching}
        {li [DefBlock(def, body)] defines a new block with [def], and executes [body]} *)
type program =
    | Return   of span * expr
    | Jump     of span * block_label * value list
    | Stmt     of span * statement * program
    | Branch   of branching
    | DefBlock of block_definition * program


(** [branching] is a simple switch on ADT label (integer tag).
    Each ADT label has an associated branch in [br_branches],
    and there is an optional default branch [br_default].

    [branching] is used to represent all control flow branching in the source language:
    {ul
        {li {e if/else} is translated to pattern matching on boolean}
        {li pattern matching is decomposed to nested [branching]}
    } *)
and branching =
    { br_src      : span
    ; br_matched  : value
    ; br_branches : (adt_label * program) list
    ; br_default  : program option }


and block_definition =
    { blk_src   : span
    ; blk_label : block_label
    ; blk_args  : variable list
    ; blk_body  : program }



type function_definition =
    { func_src  : span
    ; func_name : string
    ; func_args : variable list
    ; func_body : program }
