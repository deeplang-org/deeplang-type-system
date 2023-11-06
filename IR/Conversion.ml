type top_clause = Syntax.ParseTree.top_clause
type func_impl = Syntax.ParseTree.func_impl
type func_arg = Syntax.ParseTree.func_arg
type context = Semantics.Walker.context
type int_typ_sign = Syntax.ParseTree.int_typ_sign

let trans_expr (_context: context) (expr: Syntax.ParseTree.expr): ANF.expr = match expr.shape with
  | ExpLit(lit) -> (match lit with
    | LitUnit -> Val(Int(0))
    | LitBool(bool) -> (match bool with
      | true -> Val(Int(1))
      | false -> Val(Int(0)))
    | LitInt(int) -> Val(Int(int))
    | LitFloat(float) -> Val(Float(float))
    (** 16-bit *)
    | LitChar(int) -> Val(Int(int))
    | LitString(str) -> Val(String(str)))
  (* | ExpVar(var) -> _context.nametbl *)
  | _ -> failwith "TODO1";;

let rec trans_stmt (_context: context) (stmt: Syntax.ParseTree.stmt): ANF.program = match stmt.shape with
  | StmtSeq(stmt_list) -> (match stmt_list with
    | h::[] -> trans_stmt (_context) (h)
    | _ -> failwith "TODO2")
  | StmtExpr(expr) -> Return(expr.span, trans_expr(_context) (expr))
  | _ -> failwith "TODO3";;

let trans_func_impl (_context:context) (func_impl:func_impl): ANF.function_definition =
  let get_arg_symbol (func_arg:func_arg) = match func_arg.farg_symb with
    Symbol(symbol) -> symbol in
  let (func_decl, stmt) = func_impl in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params = List.map get_arg_symbol func_decl.func_decl_args;
      func_label = ANF.gen_label();
      func_body = trans_stmt (_context) (stmt);
    };;

let trans_top_clause (context:context) (top_clause:top_clause) : ANF.function_definition = match top_clause.shape with
  | FunctionDef(func_impl) -> trans_func_impl(context)(func_impl)
  | _ -> failwith "TODO4";;
