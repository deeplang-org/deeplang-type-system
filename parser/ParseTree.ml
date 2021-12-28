

(** This module defines the abstract syntax of deeplang. *)
(** The {e DISCUSSION ONLY} labels below are constructs that need
    further discussion/clarification.
    Here are some of the most important ones:
    - Should we make a expr-statement distinction (as in C/C++/Java),
    or use a completely expression based setting (as in Rust/OCaml)?
    - The syntax tree itself may not be very suitable to later phrases.
    For example, evaluation order is implicit,
    and it might be more desirable to use a ANF'ed IR.
    IMO there are three possible options for this problem:
    {ol
      {- use the parse tree for everything.
      But may make later phrases harder. }
      {- define another IR, and translate the parse tree to it.
      But may take up more memory. }
      {- use another IR, and perform the translation in 2
      at parsing time.
      Will make the parser more complex. } } *)

(** {1 Helper Definitions} *)

(** [node_id] is a global, unique identifier for every node in a AST.
    {e DISCUSSION WANTED}:
    - which AST sorts need a [node_id]?
    - should different AST sorts have different [node_id] types? *)
type node_id   = NodeId   of int [@@unboxed]

(** [symbol_id] is a global, unique identifier for {e declarations}.
    For every declared new name,
    there should be a unique [symbol_id] associated with it.
    {b DISCUSSION WANTED}:
    - should different kinds of declarations have different [symbol_id] types? *)
type symbol_id = SymbolId of int [@@unboxed]

(** [src_span] is a region in some source file.
    Used to report error messages *)
type src_span =
    { file  : string (** source file name. [""] if none *)
    ; row_s : int    (** row number of the start of the region *)
    ; col_s : int    (** column number of the start of the region *)
    ; row_e : int    (** row number of the end of the region *)
    ; col_e : int    (** column number of the end of the region *) }


type variable     = string
type struct_field = string
type adt_label    = string
type method_name  = string


(** {1 Types } *)

type int_typ_sign = Signed | Unsigned
type int_typ_size = ISize_8 | ISize_16 | ISize_32 | ISize_64

type float_typ_size = FSize_32 | FSize_64

type typ_name = string

(** The abstract syntax of deeplang types.
    The name is [typ] to avoid conflict with OCaml's builtin keyword.
    [typ] only declares extra data stored with each node,
    the structure of deeplang types are defined in [typ_shape]
    {e DISCUSSION WANTED}:
    - should types have a unique id like [node_id] too?
    - should types be hash-consing'ed? *)
type typ =
    { shape : typ_shape
    ; span  : src_span }

(** [typ_shape] declares the structure of deeplang types
    {e DISCUSSION WANTED}:
    - multi paramater arrow types? *)
and typ_shape =
    | TyUnit
    | TyBool
    | TyInt   of int_typ_sign * int_typ_size
    | TyFloat of float_typ_size
    | TyChar
    | TyThis
    | TyArray of typ * int
    | TyArrow of typ * typ
    | TyNamed of typ_name



(** {1 Patterns} *)

type pattern =
    { shape  : pattern_shape
    ; pat_id : node_id
    ; span   : src_span }

and pattern_shape =
    | PatWildcard
    | PatVar    of variable
    | PatADT    of adt_label * pattern list
    | PatStruct of (struct_field * pattern) list


(** {1 Expressions} *)

type literal =
    | LitBool   of bool
    | LitInt    of int
    | LitFloat  of float
    | LitString of string

(** Some notes on [BinOpAssign]:
    - [BinOpAssign None] is normal assignment
    - [BinOpAssign Add] is "+=", [BinOpAssign Sub] is "-=", etc.
    - Not all operators have corresponding assignment operators
    in the parser *)
type binary_op =
    | BinOpLt  | BinOpLeq | BinOpGt | BinOpGeq
    | BinOpEq | BinOpNeq
    | BinOpLOr | BinOpLAnd | BinOpLNot
    | BinOpLShift | BinOpRShift
    | BinOpAdd | BinOpSub | BinOpMul | BinOpDiv | BinOpMod
    | BinOpAssign of binary_op option


type expr =
    { shape   : expr_shape
    ; expr_id : node_id
    ; span    : src_span }

(** {e DISCUSSION WANTED}:
    - Should we make a expr-statement distinction (as in C/C++/Java),
    or use a completely expression based setting (as in Rust)?
    - Should we preserve the "evil" ++/-- operators? *)
and expr_shape =
    | ExpLit    of literal
    | ExpVar    of variable
    | ExpBinOp  of binary_op * expr * expr
    | ExpField  of expr * struct_field list
    | ExpApp    of expr * expr list
    | ExpNew    of typ_name * expr list
    | ExpMethod of expr * method_name * expr list



(** {1 Statements} *)

type mutability = Imm | Mut

type declaration =
    { decl_name  : string
    ; decl_id    : symbol_id
    ; decl_value : expr }

type stmt =
    { shape   : stmt_shape
    ; stmt_id : node_id
    ; span    : src_span }

(** {e DISCUSSION WANTED}:
    - expr/statement distinction, or expr only (see above)
    - for loop: range/iterator based loops? *)
and stmt_shape =
    | StmtExpr   of expr
    | StmtDecl   of declaration
    | StmtIf     of expr * stmt * stmt
    | StmtFor    of stmt * expr * stmt * stmt
    | StmtWhile  of expr * stmt
    | StmtMatch  of expr * (pattern * stmt) list
    | StmtReturn of expr

and for_loop_init =
    | ForInitDecl of declaration
    | ForInitExpr of expr



(** {1 Top Level Expressions } *)

type has_impl = HasImpl

(** [has_impl] and [no_impl]
    are used only as index of [optional_impl] below.
    Their values have no meaning and are never used. *)
type no_impl = NoImpl

(** [('a, 'kind) optional_impl] is similar to ['a option],
    but whether there is a value of type ['a] is reflected by
    the type index ['kind]:
    - When ['kind] is [no_impl], there is no value.
    - When ['kind] is [has_impl], there is a value of type ['a].

    This type is used below to simplify definition of [top_expr]. *)
type (_, _) optional_impl =
    | NoImpl  :       ('a, no_impl ) optional_impl
    | HasImpl : 'a -> ('a, has_impl) optional_impl


type top_expr =
    { shape : top_expr_shape
    ; span  : src_span }

(** The type of top level expressions. *)
and top_expr_shape =
    | DefStruct     of struct_def
    | DefADT        of adt_def
    | DeclInterface of no_impl  interface_def
    | ImplInterface of has_impl interface_def
    | DefFunction   of has_impl function_def
    | DefVariable   of variable_def

and struct_def =
    { struct_name   : typ_name
    ; struct_fields : struct_def_field list }

and struct_def_field =
    | StructField    of struct_field * typ
    | StructDelegate of struct_field * typ

and adt_def =
    { adt_name : typ_name
    ; adt_branches : (adt_label * typ) list }

(** [declaration interface_def] is an interface declaration.
    [implementation interface_def] is an interface implementation.
    {e DISCUSSION WANTED}:
    - Should we support generic interface, interface dependency, etc.? *)
and 'kind interface_def =
    { interface_name    : typ_name
    ; interface_methods : 'kind function_def list }

(** [declaration function_def] is an function declaration.
    [implementation function_def] is an function implementation.
    [func_body] will only be present for function implementations *)
and 'kind function_def =
    { func_name : variable
    ; func_args : (variable * symbol_id * typ) list
    ; func_ret  : typ
    ; func_body : (expr, 'kind) optional_impl }

and variable_def =
    { var_name  : variable
    ; var_id    : symbol_id
    ; var_value : expr }
