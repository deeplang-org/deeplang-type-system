type top_clause = Syntax.ParseTree.top_clause
type func_impl = Syntax.ParseTree.func_impl
type func_arg = Syntax.ParseTree.func_arg
type int_typ_sign = Syntax.ParseTree.int_typ_sign

type variable = ANF.variable

type var_data = 
    { 
      (* mut  : mutability
    ; typ  : typ *)
    name : int
    (* ; id   : NodeId.expr *)
    }
    ;;

type var_table = (Syntax.ParseTree.symbol * var_data) list;;


type expr_continuation =
  | Simple  of ANF.label
  | Complex of (ANF.value -> ANF.program)

type stmt_continuation = ANF.program

let apply_cont ~span k value : ANF.program =
  match k with
  | Simple label -> Jump(span, label, [ value ])
  | Complex f -> f value

let rec trans_expr
  ~var_table:(_var_table: var_table)
  (expr: Syntax.ParseTree.expr)
  (cont: expr_continuation) : ANF.program =
  match expr.shape with
  | ExpLit(LitUnit) -> apply_cont ~span:expr.span cont (Int 0)
  | ExpLit(LitBool(true)) -> apply_cont ~span:expr.span cont (Int 1)
  | ExpLit(LitBool(false)) -> apply_cont ~span:expr.span cont (Int 0)
  | ExpLit(LitInt(i)) -> apply_cont ~span:expr.span cont (Int i)
  | ExpLit(LitFloat(f)) -> apply_cont ~span:expr.span cont (Float f)
  | ExpLit(LitChar(ch)) -> apply_cont ~span:expr.span cont (Int ch)
  | ExpLit(LitString(str)) -> apply_cont ~span:expr.span cont (String str)
  | _ -> failwith "TODO0"

and trans_stmt
  ~(var_table: var_table)
  ~(return: ANF.label)
  (stmt: Syntax.ParseTree.stmt)
  (cont: stmt_continuation): ANF.program =
  match stmt.shape with
  | StmtSeq stmt_list -> trans_stmts ~var_table ~return stmt_list cont
  | StmtReturn expr ->
      (* [StmtReturn] is early return: what's behind it will not get executed.
         So the continuation is discarded *)
      trans_expr ~var_table expr (Simple return)
  (* | StmtExpr(expr) -> Return(expr.span, trans_expr(_context) (expr)) *)
  | _ -> failwith "TODO3"

and trans_stmts
  ~(var_table: var_table)
  ~(return: ANF.label)
  (stmts: Syntax.ParseTree.stmt list)
  (cont: stmt_continuation): ANF.program =
  match stmts with
  | []    -> cont
  | s::ss -> trans_stmt ~var_table ~return s (trans_stmts ~var_table ~return ss cont)

let trans_func_impl ~(var_table: var_table) (func_impl:func_impl): ANF.function_definition =
  let get_arg_symbol (func_arg:func_arg) =
    match func_arg.farg_symb with Symbol symbol -> symbol
  in
  let func_label = ANF.gen_label () in
  let (func_decl, stmt) = func_impl in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params = List.map get_arg_symbol func_decl.func_decl_args;
      func_label;
      func_body = trans_stmt ~var_table ~return:func_label stmt Empty;
    };;

let trans_top_clause (var_table:var_table) (top_clause:top_clause) : ANF.function_definition = match top_clause.shape with
  | FunctionDef func_impl -> trans_func_impl ~var_table func_impl
  | _ -> failwith "TODO4";;
