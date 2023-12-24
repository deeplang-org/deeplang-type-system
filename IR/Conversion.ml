type top_clause = Syntax.ParseTree.top_clause
type func_impl = Syntax.ParseTree.func_impl
type func_arg = Syntax.ParseTree.func_arg
type context = Semantics.Walker.context
type int_typ_sign = Syntax.ParseTree.int_typ_sign

type var_data = 
    { 
      (* mut  : mutability
    ; typ  : typ *)
    name : int
    (* ; id   : NodeId.expr *)
    }
    ;;

type var_table = (Syntax.ParseTree.symbol * var_data) list;;

let rec trans_expr_assign
  (var_table: var_table)
  (span: ANF.span)
  (op: Syntax.ParseTree.calculate_op option)
  (lvalue: Syntax.ParseTree.expr)
  (rvalue: Syntax.ParseTree.expr)
  (cont: Syntax.ParseTree.stmt list): ANF.program = match op with
    | Some(op) -> trans_expr_assign (var_table) span None lvalue {span = rvalue.span; expr_id = rvalue.expr_id; shape = ExpBinOp(BinOpCalculate(op), lvalue, rvalue)} (cont)
    | None -> failwith "TODO2"
      (* match rvalue.shape of *)
    (* | ExpLit(lit) -> (match lit with
      | LitUnit -> Val(Int(0))
      | LitBool(bool) -> (match bool with
        | true -> Val(Int(1))
        | false -> Val(Int(0)))
      | LitInt(int) -> Val(Int(int))
      | LitFloat(float) -> Val(Float(float))
      (** 16-bit *)
      | LitChar(int) -> Val(Int(int))
      | LitString(str) -> Val(String(str)))
    | ExpVar(var_name) -> (match Hashtbl.find_opt _context.nametbl var_name with
      | Some(sym) ->  (match sym with
        | Symbol(id) -> Val(Int(id)))
      | None -> failwith "find no variable")
    | _ -> failwith "TODO1";; *)

let rec trans_stmt (var_table: var_table) (stmt: Syntax.ParseTree.stmt) (cont: Syntax.ParseTree.stmt list): ANF.program = match stmt.shape with
  | StmtSeq(stmt_list) -> trans_stmts (var_table) (stmt_list) (cont)
  (* | StmtReturn(expr) -> Return(expr.span, trans_expr(_context) (expr)) *)
  (* | StmtExpr(expr) -> Return(expr.span, trans_expr(_context) (expr)) *)
  | _ -> failwith "TODO3"

and trans_stmts (var_table: var_table) (stmts: Syntax.ParseTree.stmt list) (cont: Syntax.ParseTree.stmt list): ANF.program = match (stmts @ cont) with
  | []    -> failwith "empty stmts"
  | s::ss -> trans_stmt (var_table) s ss;;

let trans_func_impl (var_table: var_table) (func_impl:func_impl): ANF.function_definition =
  let get_arg_symbol (func_arg:func_arg) = match func_arg.farg_symb with
    Symbol(symbol) -> symbol in
  let (func_decl, stmt) = func_impl in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params = List.map get_arg_symbol func_decl.func_decl_args;
      func_label = ANF.gen_label();
      func_body = trans_stmt (var_table) (stmt) ([]);
    };;

let trans_top_clause (var_table:var_table) (top_clause:top_clause) : ANF.function_definition = match top_clause.shape with
  | FunctionDef(func_impl) -> trans_func_impl(var_table)(func_impl)
  | _ -> failwith "TODO4";;
