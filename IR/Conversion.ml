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

(* let rec trans_expr
  (var_table: var_table)
  (assignee: variable)
  (expr: Syntax.ParseTree.expr)
  (cont: ANF.program -> ANF.program): ANF.program -> ANF.program = fun program -> match expr.shape with
    | ExpLit(LitUnit) -> Stmt(expr.span, Decl(assignee, Val(Int(0))), trans_stmts(var_table)(cont))
    | ExpLit(LitBool(true)) -> Stmt(expr.span, Decl(assignee, Val(Int(1))), trans_stmts(var_table)(cont))
    | ExpLit(LitBool(false)) -> Stmt(expr.span, Decl(assignee, Val(Int(0))), trans_stmts(var_table)(cont))
    | ExpLit(LitInt(i)) -> Stmt(expr.span, Decl(assignee, Val(Int(i))), trans_stmts(var_table)(cont))
    | ExpLit(LitFloat(f)) -> Stmt(expr.span, Decl(assignee, Val(Float(f))), trans_stmts(var_table)(cont))
    | ExpLit(LitChar(ch)) -> Stmt(expr.span, Decl(assignee, Val(Int(ch))), trans_stmts(var_table)(cont))
    | ExpLit(LitString(str)) -> Stmt(expr.span, Decl(assignee, Val(String(str))), trans_stmts(var_table)(cont))
    | _ -> failwith "TODO0" *)

let rec trans_stmt
  (var_table: var_table)
  (stmt: Syntax.ParseTree.stmt)
  (cont: ANF.program -> ANF.program): ANF.program -> ANF.program = fun program -> match stmt.shape with
  | StmtSeq(stmt_list) -> trans_stmts var_table stmt_list cont program
  (* | StmtReturn(expr) -> Stmt(expr.span, trans_expr (ANF.gen_var()) (expr)) Return(expr.span, trans_expr(_context) (expr)) *)
  (* | StmtExpr(expr) -> Return(expr.span, trans_expr(_context) (expr)) *)
  | _ -> failwith "TODO3"

and trans_stmts
  (var_table: var_table)
  (stmts: Syntax.ParseTree.stmt list)
  (cont: ANF.program -> ANF.program): ANF.program -> ANF.program = fun program -> match stmts with
  | []    -> program
  | s::ss -> trans_stmt var_table s (trans_stmts var_table ss cont) program;;

let trans_func_impl (var_table: var_table) (func_impl:func_impl): ANF.function_definition =
  let get_arg_symbol (func_arg:func_arg) = match func_arg.farg_symb with
    Symbol(symbol) -> symbol in
  let (func_decl, stmt) = func_impl in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params = List.map get_arg_symbol func_decl.func_decl_args;
      func_label = ANF.gen_label();
      func_body = trans_stmt var_table stmt (fun x -> x) Empty;
    };;

let trans_top_clause (var_table:var_table) (top_clause:top_clause) : ANF.function_definition = match top_clause.shape with
  | FunctionDef(func_impl) -> trans_func_impl(var_table)(func_impl)
  | _ -> failwith "TODO4";;
