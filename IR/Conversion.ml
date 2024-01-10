type top_clause = Syntax.ParseTree.top_clause
type func_impl = Syntax.ParseTree.func_impl
type func_arg = Syntax.ParseTree.func_arg
type int_typ_sign = Syntax.ParseTree.int_typ_sign

type variable = ANF.variable

type var_data = 
    { 
      (* mut  : mutability
    ; typ  : typ *)
    name : ANF.variable
    (* ; id   : NodeId.expr *)
    }
    ;;

type var_table = (string * var_data) list;;

let var_to_value ~src (var : ANF.variable) : ANF.value =
  LVal { lv_var = var; lv_path = []; lv_src = src }


(* To convert AST to ANF, we use continuation-passing-style during the conversion.
   Translation of every AST node receives an extra "continuation" parameter,
   which reperest what remains to be done after translating current AST node.
   For example, when translating the following expression:

      x + 2

   The continuation of [x] is [_ + 2], where [_] represents a hole to plug values in.

   In most cases, translation of AST node simply feed its result to the continuation,
   obtaining the remaining part of the whole program,
   and prepend codes of the AST node itself to the remaining part of the program.

   But in a few special cases, in particular control flow constructs like [if],
   the translation may capture the continuation and put it in a ANF block.
   See the concrete translation code below for more details. *)


(* [expr_continuation] is the continuation of an expression.
   The continuation accepts an value, which is the result of the expression,
   and produce a complete program.

   [expr_continuation] is conceptually just a function [ANF.value -> ANF.program].
   However for the special cases where the continuation is [fun value -> Jump(label, value)],
   we represent it as [Simple label].
   This is used to simplify the generated ANF when translating control flow constructs. *)
type expr_continuation =
  | Simple  of ANF.label
  | Complex of (ANF.value -> ANF.program)

(* feed a value to an [expr_continuation] and obtain a complete program *)
let apply_expr_cont ~span k value : ANF.program =
  match k with
  | Simple label -> Jump(span, label, [ value ])
  | Complex f -> f value

(* [stmt_continuation] is similar to [expr_continuation],
   except that it is the continuation for statements.
   Since statements do not have a result value, it does not need an value as argument.
   However, statements may declare new variables,
   so [stmt_continuation] is conceptually a function [var_table -> ANF.program].
   Again, for the special case where the continuation is just [fun _ -> Jump(label, [])],
   we represent it as [Simple label]. *)
type stmt_continuation =
  | Simple of ANF.span * ANF.label
  | Complex of (var_table -> ANF.program)

let apply_stmt_cont k ~var_table : ANF.program =
  match k with
  | Simple (span, label) -> Jump(span, label, [])
  | Complex f -> f var_table


let bind (value : ANF.value) (f : ANF.variable -> ANF.program) : ANF.program =
  match value with
  | LVal{ lv_var; lv_path = []; lv_src = _ } -> f lv_var
  | _ ->
      let var = ANF.gen_var () in
      Stmt( Syntax.SyntaxError.dummy_span, Decl(var, Val value), f var)

let rec trans_expr
  ~(var_table: var_table)
  (expr: Syntax.ParseTree.expr)
  (cont: expr_continuation) : ANF.program =
  match expr.shape with
  | ExpLit lit -> apply_expr_cont ~span:expr.span cont (ConvertMatch.trans_lit lit)
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
  ~(table : Semantics.Table.table)
  ~(var_table: var_table)
  (* [return] is the label of current function *)
  ~(return: ANF.label)
  (stmt: Syntax.ParseTree.stmt)
  (cont: stmt_continuation): ANF.program =
  match stmt.shape with
  | StmtSeq stmt_list ->
      let cont' =
        match cont with
        | Simple _ -> cont
        | Complex f ->
            (* [StmtSeq] opens a new scope.
               So after translating [stmt_list],
               we should discard the new variables declared in [stmt_list]. *)
            Complex (fun _ -> f var_table)
      in
      trans_stmts ~table ~var_table ~return stmt_list cont'
  | StmtExpr expr ->
      trans_expr ~var_table expr (Complex (fun _ ->
          (* the result of [StmtExpr] is unused, discard it *)
          apply_stmt_cont cont ~var_table))
  | StmtReturn expr ->
      (* [StmtReturn] is early return: what's behind it will never get executed.
         So the continuation is discarded *)
      trans_expr ~var_table expr (Simple return)
  | StmtDecl ({ shape = PatVar vpat;_ }, rhs) ->
      (* TODO: handle all patterns *)
      trans_expr ~var_table rhs (Complex (fun rhs_value ->
          match rhs_value with
          | LVal { lv_var; lv_path = []; lv_src = _ } when vpat.vpat_mut = Imm ->
              (* if [rhs_value] is also a variable,
                 we can reuse this variable and don't have to generate a new declaration.

                 However, this only applies to immutable variables.
                 For mutable variables, we must always create a new ANF variable.

                 FIXME: [lv_var] must be immutable too. check for it *)
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
            let conseq = trans_stmt ~table ~var_table ~return conseq k in
            let alter =
              match alter with
              | Some alter -> trans_stmt ~table ~var_table ~return alter k
              | None -> apply_stmt_cont k ~var_table
            in
            Branch {
              br_src = stmt.span;
              br_matched = cond_value;
              br_branches = [ (1, conseq) ];
              br_default = Some alter;
            }
          in
          (* The continuation [k] in [trans_if] above may be used TWICE,
             once for translating [conseq] and once for translating [alter].
             This would result in code in [k] being duplicated, which is bad.

             To solve this problem, we capture [cont] and store it in a block.
             Now, the two branches of [if] simply jumps to the new block,
             and the code of [cont] is reused through the new block. *)
          match cont with
          | Simple _ ->
              (* when the continuation is just a jump,
                 there's no need to create a new block. *)
              trans_if cont
          | Complex f ->
              let label = ANF.gen_label () in
              let block : ANF.block_definition =
                {
                  blk_label = label;
                  blk_params = [];
                  blk_body =
                    (* continuation captured here *)
                    f var_table;
                }
              in
              (* this time, the continuation passed to [trans_if] is just a simple jump
                 and there is no code duplication problem anymore *)
              Block(block, trans_if (Simple (stmt.span, label)))))
  | StmtWhile (cond, body) ->
      (* while cond { body }; rest

        ==>
        block #break_loop {
          loop #cont_loop {
            if cond {
              body;
              jump #cont_loop;
            } else {
              jump #break_loop;
            }
          }
        } rest *)
      let label_break = ANF.gen_label () in
      let label_cont = ANF.gen_label () in
      let loop_body =
        trans_expr ~var_table cond (Complex (fun cond_value ->
            Branch {
              br_src = stmt.span;
              br_matched = cond_value;
              br_branches =
                [ (1, trans_stmt ~table ~var_table ~return body (Simple (stmt.span, label_cont))) ];
              br_default = Some ( Jump(stmt.span, label_break, []));
            }))
      in
      Block
        ( { blk_label = label_break
          ; blk_params = []
          ; blk_body = Loop { blk_label = label_cont; blk_params = []; blk_body = loop_body } }
        , (apply_stmt_cont cont ~var_table))
  | StmtMatch (head, arms) ->
      let trans_match k =
        let rec bindings_of_pat acc (pat : Syntax.ParseTree.pattern) =
          match pat.shape with
          | PatWildcard | PatLit _ -> acc
          | PatVar vpat -> vpat.vpat_name :: acc
          | PatAs(pat', vpat) -> bindings_of_pat (vpat.vpat_name :: acc) pat'
          | PatTuple pats | PatADT(_, pats) ->
              List.fold_left bindings_of_pat acc pats
          | PatStruct(_, field_pats) ->
              List.fold_left (fun acc (_, pat) -> bindings_of_pat acc pat) acc field_pats
        in
        let action_blocks =
          arms |> List.map (fun (pat, (action : Syntax.ParseTree.stmt)) ->
            let bindings = bindings_of_pat [] pat in
            let blk_params = List.map (fun _ -> ANF.gen_var ()) bindings in
            let var_table =
              List.map2 (fun name var -> (name, { name = var })) bindings blk_params
              @ var_table
            in
            (bindings,
              ANF.{
                blk_label = ANF.gen_label ();
                blk_params;
                blk_body = trans_stmt ~table ~var_table ~return action k;
              }))
        in
        let trans_arms =
          List.map2
            (fun
              (params, (block : ANF.block_definition))
              (pat, (action : Syntax.ParseTree.stmt)) ->
                  (pat,
                    fun bindings ->
                      let args =
                        List.map
                          (fun param ->
                              var_to_value ~src:Syntax.SyntaxError.dummy_span
                                (List.assoc param bindings))
                          params
                      in
                      ANF.Jump(action.span, block.blk_label, args)))
            action_blocks arms
        in
        let body =
          trans_expr ~var_table head (Complex (fun head_value ->
              bind head_value (fun head ->
                ConvertMatch.trans_match ~table head trans_arms)))
        in
        List.fold_right (fun (_, block) body -> ANF.Block(block, body)) action_blocks body
      in
      begin match cont with
      | Simple _ -> trans_match cont
      | Complex f ->
          let blk_label = ANF.gen_label () in
          Block({ blk_label; blk_params = []; blk_body = f var_table },
            trans_match (Simple (stmt.span, blk_label)))
      end
  | StmtBreak -> failwith "TODO Break"
  | StmtContinue -> Jump(stmt.span, return, [])
  | _ -> failwith "TODO3"

and trans_stmts
  ~(table : Semantics.Table.table)
  ~(var_table: var_table)
  ~(return: ANF.label)
  (stmts: Syntax.ParseTree.stmt list)
  (cont: stmt_continuation): ANF.program =
  match stmts with
  | []    -> apply_stmt_cont cont ~var_table
  | s::ss ->
      trans_stmt ~table ~var_table ~return s (Complex (fun var_table ->
          trans_stmts ~table ~var_table ~return ss cont))

let trans_func_impl ~(table : Semantics.Table.table)
    (func_impl:func_impl): ANF.function_definition =
  let func_label = ANF.gen_label () in
  let (func_decl, stmt) = func_impl in
  let func_params = List.map (fun _ -> ANF.gen_var ()) func_decl.func_decl_args in
  let var_table =
    List.map2 (fun farg var -> (farg.Syntax.ParseTree.farg_name, { name = var }))
      func_decl.func_decl_args func_params
  in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params;
      func_label;
      func_body =
        trans_stmt ~table ~var_table ~return:func_label stmt (Simple (stmt.span, func_label));
    };;

let trans_program ~table (program : top_clause list)
    : ANF.function_definition list =
  List.filter_map
    (fun (top_clause : top_clause) ->
          match top_clause.shape with
          | FunctionDef func_impl -> Some (trans_func_impl ~table func_impl)
          | ADTDef _ | StructDef _ -> None
          | _ -> failwith "TODO4")
    program
