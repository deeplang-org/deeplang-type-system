
type variable = int
let pp_variable fmt var = Format.fprintf fmt "$%d" var

type label = int
let pp_label fmt lbl = Format.fprintf fmt "#%d" lbl

type func_name = Syntax.ParseTree.func_name [@@deriving show]
type adt_label = Syntax.ParseTree.adt_label [@@deriving show]
type typ_name  = Syntax.ParseTree.typ_name [@@deriving show]
type intf_name = Syntax.ParseTree.intf_name [@@deriving show]

type unary_op  = Syntax.ParseTree.unary_op [@@deriving show]
type binary_op = Syntax.ParseTree.binary_op [@@deriving show]

type span = Syntax.SyntaxError.src_span [@@deriving show]


type mutability = Imm | Mut [@@deriving show]
type permission = Owner | Borrow of mutability [@@deriving show]



(** [data_kind] is used to classify different kinds of aggregate data.
    In the ANF IR, different kinds of aggregate data share a uniform representation. *)
type data_kind =
    | Tuple  of int
    | Struct of typ_name
    | ADT    of typ_name * adt_label
    | Impl   of intf_name (** method dict/vtable of interface *)
    [@@deriving show]



(** [lvalue] is a value that may appear at the left hand side of an assignment.
    So every [lvalue] corresponds to a memory address.
    Some examples: [point.x], [*ptr] *)
type lvalue =
    { lv_var  : variable
    ; lv_path : path
    ; lv_src  : span }
    [@@deriving show]

and path = path_node list
    [@@deriving show]

(** [path_node] is used to select a part of an existing [lvalue]:
    {ul
        {li [Field k] selects the [k]-th field from an aggregate,
        i.e. a tuple, a struct, an ADT or a method dictionary}
        {li [Deref] selects the address a pointer points to}
        {li [AsTag l] selects the data associated with label [l] in an ADT}
        {li [Method m] selects the implementation of method [m] from an interface implementation}
    } *)
and path_node =
    | Field  of int
    | Deref
    | AsTag  of int
    | Method of string
    [@@deriving show]

(** A [value] in the ANF IR is something immediately available without needing
    any computation. *)
type value =
    | LVal  of lvalue
    | Int   of int
    | Float of float
    | String of string
    [@@deriving show]

type expr =
    | Val    of value
    | Copy   of lvalue
    | Borrow of mutability * lvalue
    | App    of value * value list
    | UnOp   of unary_op  * value
    | BinOp  of binary_op * value * value
    | MkData of data_kind * value list
    | Fun    of func_name
    [@@deriving show]

type statement =
    | Decl     of variable * expr
    | Assign   of lvalue * value
    | EndScope of variable list
    [@@deriving show]

(** {ul
        {li [Jump(span, label, args)] jumps to the block with name [label] with [args]}
        {li [Stmt(span, stmt, body)] first executes [stmt] and then executes [body].
            [span] is the source location of [stmt]}
        {li [Branch br] is a unified construction for all control flow branching structures,
            e.g. if/else and pattern matching}
        {li [Block(def, body)] defines a new block with [def], and executes [body]}
        {li [Loop def] defines a {e recursive} block with [def],
        and immediately enter the block}
    } *)
type program =
    | Jump   of span * label * value list
    | Stmt   of span * statement * program
    | Branch of branching
    | Block  of block_definition * program
    | Loop   of block_definition
    | Empty
    | Abort
    [@@deriving show]

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
    ; br_branches : (int * program) list
    ; br_default  : program option }
    [@@deriving show]

and block_definition =
    { blk_label  : label
    ; blk_params : variable list
    ; blk_body   : program }
    [@@deriving show]

(** [func_label] represents the point {e after} the function returns.
    Returning from the function is represented by jumping to [func_label]. *)
type function_definition =
    { func_src    : span
    ; func_name   : string
    ; func_params : variable list
    ; func_label  : label
    ; func_body   : program }
    [@@deriving show]

let (gen_var, gen_label, reset_generator) =
    let var_seed = ref 0 in
    let label_seed = ref 0 in
    ( (fun () -> incr var_seed; !var_seed)
    , (fun () -> incr label_seed; !label_seed)
    , (fun () -> var_seed := 0; label_seed := 0) )

(* Concatenate two programs *)
let rec concat_program(p1: program)(p2: program): program =
    match p1 with
    | Stmt(span, stmt, body) -> Stmt(span, stmt, concat_program body p2)
    | Block(def, body) -> Block(def, concat_program body p2)
    | _ -> p1 (* In the other cases, the second program is unreachable *)
