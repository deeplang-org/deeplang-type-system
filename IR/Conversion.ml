type top_clause = Syntax.ParseTree.top_clause
type func_impl = Syntax.ParseTree.func_impl
type func_arg = Syntax.ParseTree.func_arg
type context = Semantics.Walker.context
type int_typ_sign = Syntax.ParseTree.int_typ_sign

let trans_func_impl (_context:context) (func_impl:func_impl): ANF.function_definition =
  let get_arg_symbol (func_arg:func_arg) = match func_arg.farg_symb with
    Symbol(symbol) -> symbol in
  let (func_decl, stmt) = func_impl in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params = List.map get_arg_symbol func_decl.func_decl_args;
      func_label = ANF.gen_label();
      func_body = Return(stmt.span, Val(Int(114514)));
    };;

let trans_top_clause (context:context) (top_clause:top_clause) : ANF.function_definition = match top_clause.shape with
  | FunctionDef(func_impl) -> trans_func_impl(context)(func_impl)
  | _ -> failwith "TODO";;
