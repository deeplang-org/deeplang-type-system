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

type var_table = (string * var_data) list;;

let var_to_value ~src (var : ANF.variable) : ANF.value =
  LVal { lv_var = var; lv_path = []; lv_src = src }

type expr_continuation =
  | Simple  of ANF.label
  | Complex of (ANF.value -> ANF.program)

let apply_expr_cont ~span k value : ANF.program =
  match k with
  | Simple label -> Jump(span, label, [ value ])
  | Complex f -> f value

type stmt_continuation =
  | Simple of ANF.span * ANF.label
  | Complex of (var_table -> ANF.program)

let apply_stmt_cont k ~var_table : ANF.program =
  match k with
  | Simple (span, label) -> Jump(span, label, [])
  | Complex f -> f var_table

let trans_lit (lit : Syntax.ParseTree.literal) : ANF.value =
  match lit with
  | LitUnit       -> Int 0
  | LitBool true  -> Int 1
  | LitBool false -> Int 0
  | LitInt i      -> Int i
  | LitFloat f    -> Float f
  | LitChar ch    -> Int ch
  | LitString str -> String str

let rec trans_expr
  ~(var_table: var_table)
  (expr: Syntax.ParseTree.expr)
  (cont: expr_continuation) : ANF.program =
  match expr.shape with
  | ExpLit lit -> apply_expr_cont ~span:expr.span cont (trans_lit lit)
  | ExpVar var ->
      apply_expr_cont ~span:expr.span cont
        (var_to_value ~src:expr.span (List.assoc var var_table).name)
  | ExpBinOp (op, lhs, rhs) ->
      trans_expr ~var_table lhs (Complex (fun lhs_value ->
          trans_expr ~var_table rhs (Complex (fun rhs_value ->
              let result_var = ANF.gen_var () in
              Stmt( expr.span, Decl(result_var, BinOp(op, lhs_value, rhs_value)),
                apply_expr_cont ~span:expr.span cont
                  (var_to_value ~src:expr.span result_var))))))
  | _ -> failwith "TODO0"

and trans_stmt
  ~(var_table: var_table)
  ~(return: ANF.label)
  (stmt: Syntax.ParseTree.stmt)
  (cont: stmt_continuation): ANF.program =
  match stmt.shape with
  | StmtSeq stmt_list ->
      let cont' =
        match cont with
        | Simple _ -> cont
        | Complex f -> Complex (fun _ -> f var_table)
      in
      trans_stmts ~var_table ~return stmt_list cont'
  | StmtExpr expr ->
      trans_expr ~var_table expr (Complex (fun _ -> apply_stmt_cont cont ~var_table))
  | StmtReturn expr ->
      (* [StmtReturn] is early return: what's behind it will not get executed.
         So the continuation is discarded *)
      trans_expr ~var_table expr (Simple return)
  | StmtDecl ({ shape = PatVar vpat;_ }, rhs) ->
      (* TODO: handle all patterns *)
      trans_expr ~var_table rhs (Complex (fun rhs_value ->
          match rhs_value with
          | LVal { lv_var; lv_path = []; lv_src = _ } when vpat.vpat_mut = Imm ->
              let new_var_table = (vpat.vpat_name, { name = lv_var }) :: var_table in
              apply_stmt_cont cont ~var_table:new_var_table
          | _ ->
              let anf_var = ANF.gen_var () in
              let new_var_table = (vpat.vpat_name, { name = anf_var }) :: var_table in
              Stmt ( stmt.span, Decl (anf_var, Val rhs_value),
                apply_stmt_cont cont ~var_table:new_var_table)))
  | StmtIf (cond, conseq, alter) ->
      trans_expr ~var_table cond (Complex (fun cond_value ->
          let[@inline] trans_if (k : stmt_continuation) : ANF.program =
            let conseq = trans_stmt ~var_table ~return conseq k in
            let alter =
              match alter with
              | Some alter -> trans_stmt ~var_table ~return alter k
              | None -> Empty
            in
            Branch {
              br_src = stmt.span;
              br_matched = cond_value;
              br_branches = [ (1, conseq) ];
              br_default = Some alter;
            }
          in
          match cont with
          | Simple _ -> trans_if cont
          | Complex f ->
              let label = ANF.gen_label () in
              let block : ANF.block_definition =
                {
                  blk_label = label;
                  blk_params = [];
                  blk_body = f var_table;
                }
              in
              Block(block, trans_if (Simple (stmt.span, label)))))
  | _ -> failwith "TODO3"

and trans_stmts
  ~(var_table: var_table)
  ~(return: ANF.label)
  (stmts: Syntax.ParseTree.stmt list)
  (cont: stmt_continuation): ANF.program =
  match stmts with
  | []    -> apply_stmt_cont cont ~var_table
  | s::ss ->
      trans_stmt ~var_table ~return s (Complex (fun var_table ->
          trans_stmts ~var_table ~return ss cont))

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
      func_body = trans_stmt ~var_table ~return:func_label stmt (Simple (stmt.span, func_label));
    };;

let trans_top_clause (var_table:var_table) (top_clause:top_clause) : ANF.function_definition = match top_clause.shape with
  | FunctionDef func_impl -> trans_func_impl ~var_table func_impl
  | _ -> failwith "TODO4";;
