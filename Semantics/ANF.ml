
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


type loc_expr =
    { var  : variable
    ; path : path }

and path = path_node list

and path_node =
    | Field of int
    | Deref


type data_kind =
    | Tuple  of int
    | Struct of typ_name
    | ADT    of typ_name * adt_label
    | Impl   of intf_name

type value =
    | Int   of int
    | Float of float
    | Loc   of permission * loc_expr
    | Fun   of func_name

type expr =
    | Val    of value
    | App    of value * value list
    | UnOp   of unary_op  * value
    | BinOp  of binary_op * value * value
    | MkData of data_kind * value list

type statement =
    | Decl     of variable * expr
    | Assign   of loc_expr * value
    | EndScope of variable list


type program =
    | Return   of span * expr
    | Jump     of span * block_label * value list
    | Stmt     of span * statement * program
    | Branch   of branching
    | DefBlock of block_definition * program

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
