type top_clause = Syntax.ParseTree.top_clause
type func_impl = Syntax.ParseTree.func_impl
type func_arg = Syntax.ParseTree.func_arg
type int_typ_sign = Syntax.ParseTree.int_typ_sign

type variable = ANF.variable

type var_data =
    { name : ANF.variable
    ; var_typ : Syntax.ParseTree.typ option
    }
    ;;

type var_table = (string * var_data) list;;

let var_to_value ~src (var : ANF.variable) : ANF.value =
  LVal { lv_var = var; lv_path = []; lv_src = src }

(** [lookup_var_type var_table name] looks up the type of a variable in the var_table *)
let lookup_var_type (var_table : var_table) (name : string) : Syntax.ParseTree.typ option =
  try (List.assoc name var_table).var_typ with Not_found -> None

(** [lookup_var_name var_table name] safely looks up the ANF name of a variable *)
let lookup_var_name (var_table : var_table) (name : string) : ANF.variable =
  try (List.assoc name var_table).name with Not_found ->
    let v = ANF.gen_var () in v


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
type 'v expr_continuation =
  | Simple  of ANF.label
  | Complex of ('v -> ANF.program)

(* feed a value to an [expr_continuation] and obtain a complete program *)
let apply_expr_cont ~span k value : ANF.program =
  match k with
  | Simple label -> Jump(span, label, [ value ])
  | Complex f -> f value

let apply_expr_list_cont ~span (k : ANF.value list expr_continuation) values : ANF.program =
  match k with
  | Simple label -> Jump(span, label, values)
  | Complex f -> f values

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

let rec traverse_expr
  ~(trans_worker: Syntax.ParseTree.expr -> ANF.value expr_continuation -> ANF.program)
  ~span
  (expr_list: Syntax.ParseTree.expr list)
  (cont: ANF.value list expr_continuation) : ANF.program = match expr_list with
    | [] -> apply_expr_list_cont ~span cont []
    | expr::rest -> trans_worker expr (
        Complex (fun expr_value -> traverse_expr ~trans_worker ~span rest (
          Complex (fun rest_values -> apply_expr_list_cont ~span cont (expr_value::rest_values))
      )))

let rec trans_expr
  ~(table : Semantics.Table.table)
  ~(var_table: var_table)
  (expr: Syntax.ParseTree.expr)
  (cont: ANF.value expr_continuation) : ANF.program =
  match expr.shape with
  | ExpLit lit -> apply_expr_cont ~span:expr.span cont (ConvertMatch.trans_lit lit)
  | ExpVar var ->
      apply_expr_cont ~span:expr.span cont
        (var_to_value ~src:expr.span (lookup_var_name var_table var))
  | ExpBinOp (op, lhs, rhs) ->
      trans_expr ~table ~var_table lhs (Complex (fun lhs_value ->
          trans_expr ~table ~var_table rhs (Complex (fun rhs_value ->
              let result_var = ANF.gen_var () in
              Stmt( expr.span, Decl(result_var, BinOp(op, lhs_value, rhs_value)),
                apply_expr_cont ~span:expr.span cont
                  (var_to_value ~src:expr.span result_var))))))
  | ExpUnOp (op, unval) ->
      (match op with
       | UnOpPreInc | UnOpPreDec ->
           (* Pre-inc/dec: read, add/sub 1, write back, return new value *)
           let calc_op = match op with UnOpPreInc -> Syntax.ParseTree.BinOpCalculate Syntax.ParseTree.BinOpAdd | _ -> Syntax.ParseTree.BinOpCalculate Syntax.ParseTree.BinOpSub in
           let name = match unval.shape with ExpVar n -> n | _ -> failwith "impossible" in
           let anf_name = lookup_var_name var_table name in
           let lv : ANF.lvalue = { lv_var = anf_name; lv_path = []; lv_src = expr.span } in
           let tmp_var = ANF.gen_var () in
           let result_var = ANF.gen_var () in
           let one_val = ANF.Int 1 in
           Stmt(expr.span, Decl(tmp_var, Val (LVal lv)),
             Stmt(expr.span, Decl(result_var, BinOp(calc_op, var_to_value ~src:expr.span tmp_var, one_val)),
               Stmt(expr.span, Assign(lv, var_to_value ~src:expr.span result_var),
                 apply_expr_cont ~span:expr.span cont (var_to_value ~src:expr.span result_var))))
       | _ ->
           trans_expr ~table ~var_table unval (Complex (fun un_value ->
               let result_var = ANF.gen_var () in
               Stmt(expr.span, Decl(result_var, UnOp(op, un_value)),
                   apply_expr_cont ~span:expr.span cont (var_to_value ~src:expr.span result_var)
               ))))
  | ExpTuple elems ->
    traverse_expr ~trans_worker:(trans_expr ~table ~var_table) ~span:expr.span elems (Complex 
      (fun value_list -> 
          let result_var = ANF.gen_var () in
          Stmt(expr.span, Decl(result_var, MkData(Tuple(List.length(value_list)), value_list)),
          apply_expr_cont ~span:expr.span cont (var_to_value ~src:expr.span result_var))
    ))
  | ExpADT (label, elems) ->
    traverse_expr ~trans_worker:(trans_expr ~table ~var_table) ~span:expr.span elems (Complex
      (fun value_list ->
        let result_var = ANF.gen_var () in
        let label_info = Hashtbl.find table.adt label in
        let sum_typ_name = label_info.sum in
        Stmt(expr.span, Decl(result_var, MkData(ADT(sum_typ_name, label), value_list)),
        apply_expr_cont ~span:expr.span cont (var_to_value ~src:expr.span result_var))
    ))
  | ExpNew (name, elems) ->
    traverse_expr ~trans_worker:(trans_expr ~table ~var_table) ~span:expr.span elems (Complex
      (fun value_list ->
        let result_var = ANF.gen_var () in
        Stmt(expr.span, Decl(result_var, MkData(Struct(name), value_list)),
        apply_expr_cont ~span:expr.span cont (var_to_value ~src:expr.span result_var))
    ))
  | ExpStruct (name, tagged_elems) ->
    let elems = List.map (fun (_, e) -> e) tagged_elems in
    traverse_expr ~trans_worker:(trans_expr ~table ~var_table) ~span:expr.span elems (Complex
      (fun value_list ->
        let result_var = ANF.gen_var () in
        Stmt(expr.span, Decl(result_var, MkData(Struct(name), value_list)),
        apply_expr_cont ~span:expr.span cont (var_to_value ~src:expr.span result_var))
    ))
  | ExpThis ->
      apply_expr_cont ~span:expr.span cont
        (var_to_value ~src:expr.span (lookup_var_name var_table "this"))
  | ExpApp (func, args) ->
      traverse_expr ~trans_worker:(trans_expr ~table ~var_table) ~span:expr.span args (Complex
        (fun arg_values ->
            let func_var = ANF.gen_var () in
            let result_var = ANF.gen_var () in
            Stmt(expr.span, Decl(func_var, ANF.Fun func),
            Stmt(expr.span, Decl(result_var, ANF.App(var_to_value ~src:expr.span func_var, arg_values)),
              apply_expr_cont ~span:expr.span cont (var_to_value ~src:expr.span result_var)))))
  | ExpField (obj, field) ->
      trans_expr ~table ~var_table obj (Complex (fun obj_value ->
          bind obj_value (fun obj_var ->
              let lv = { ANF.lv_var = obj_var; lv_path = [FieldByName field]; lv_src = expr.span } in
              apply_expr_cont ~span:expr.span cont (LVal lv))))
  | ExpMethod (obj, meth, args) ->
      (* Static method dispatch: look up method in the object's type *)
      trans_expr ~table ~var_table obj (Complex (fun obj_value ->
          traverse_expr ~trans_worker:(trans_expr ~table ~var_table) ~span:expr.span args (Complex
            (fun arg_values ->
                bind obj_value (fun obj_var ->
                    let meth_lv = { ANF.lv_var = obj_var; lv_path = [Method meth]; lv_src = expr.span } in
                    let meth_var = ANF.gen_var () in
                    let result_var = ANF.gen_var () in
                    Stmt(expr.span, Decl(meth_var, Val(LVal meth_lv)),
                    Stmt(expr.span, Decl(result_var, ANF.App(var_to_value ~src:expr.span meth_var,
                      (LVal { lv_var = obj_var; lv_path = []; lv_src = expr.span }) :: arg_values)),
                      apply_expr_cont ~span:expr.span cont (var_to_value ~src:expr.span result_var))))))))
  | ExpIf (cond, fst, snd) ->
      trans_expr ~table ~var_table cond (Complex (fun cond_value ->
          let merge_label = ANF.gen_label () in
          let result_var = ANF.gen_var () in
          let merge_block = ANF.{
            blk_label = merge_label;
            blk_params = [result_var];
            blk_body = apply_expr_cont ~span:expr.span cont
              (var_to_value ~src:expr.span result_var)
          } in
          let trans_branch branch_expr =
            trans_expr ~table ~var_table branch_expr (Simple merge_label)
          in
          Block(merge_block, Branch {
            br_src = expr.span;
            br_matched = cond_value;
            br_branches = [ (1, trans_branch fst);
                          (0, trans_branch snd) ];
            br_default = None
          })))
  | ExpMatch (head, arms) ->
      let trans_match k =
        let rec bindings_of_pat acc (pat : Syntax.ParseTree.pattern) =
          match pat.shape with
          | PatWildcard | PatLit _ -> acc
          | PatVar vpat -> vpat.vpat_name :: acc
          | PatAs(pat', vpat) -> bindings_of_pat (vpat.vpat_name :: acc) pat'
          | PatAnn(pat', _) -> bindings_of_pat acc pat'
          | PatMut(pat') -> bindings_of_pat acc pat'
          | PatTuple pats | PatADT(_, pats) ->
              List.fold_left bindings_of_pat acc pats
          | PatStruct(_, field_pats) ->
              List.fold_left (fun acc (_, pat) -> bindings_of_pat acc pat) acc field_pats
        in
        let action_blocks =
          arms |> List.map (fun (pat, (action : Syntax.ParseTree.expr)) ->
            let bindings = bindings_of_pat [] pat in
            let blk_params = List.map (fun _ -> ANF.gen_var ()) bindings in
            let branch_var_table =
              List.map2 (fun name var -> (name, { name = var; var_typ = None })) bindings blk_params
              @ var_table
            in
            (bindings,
              ANF.{
                blk_label = ANF.gen_label ();
                blk_params;
                blk_body = trans_expr ~table ~var_table:branch_var_table action k;
              }))
        in
        let trans_arms =
          List.map2
            (fun
              (params, (block : ANF.block_definition))
              (pat, (action : Syntax.ParseTree.expr)) ->
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
          trans_expr ~table ~var_table head (Complex (fun head_value ->
              bind head_value (fun head ->
                ConvertMatch.trans_match ~table head trans_arms)))
        in
        List.fold_right (fun (_, block) body -> ANF.Block(block, body)) action_blocks body
      in
      begin match cont with
      | Simple _ -> trans_match cont
      | Complex f ->
          let merge_label = ANF.gen_label () in
          let result_var = ANF.gen_var () in
          Block(ANF.{ blk_label = merge_label; blk_params = [result_var];
            blk_body = f (var_to_value ~src:expr.span result_var) },
            trans_match (Simple merge_label))
      end

and trans_stmt
  ~(table : Semantics.Table.table)
  ~(var_table: var_table)
  (* [labels] is the list of labels that encapsulates the current scope.
     For example, new labels are introduced whenever a while loop is encountered *)
  ~(labels: ANF.label list)
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
      trans_stmts ~table ~var_table ~labels ~return stmt_list cont'
  | StmtExpr expr ->
      trans_expr ~table ~var_table expr (Complex (fun _ ->
          (* the result of [StmtExpr] is unused, discard it *)
          apply_stmt_cont cont ~var_table))
  | StmtReturn expr ->
      (* [StmtReturn] is early return: what's behind it will never get executed.
         So the continuation is discarded *)
      trans_expr ~table ~var_table expr (Simple return)
  | StmtDecl (pat, rhs) when (match pat.shape with PatVar _ | PatMut({shape=PatVar _;_}) | PatAnn({shape=PatVar _;_}, _) -> true | _ -> false) ->
      (* Handle simple variable declarations, possibly wrapped in PatMut or PatAnn *)
      let rec unwrap_var (p : Syntax.ParseTree.pattern) : Syntax.ParseTree.var_pattern option =
        match p.shape with
        | PatVar vpat -> Some vpat
        | PatMut(p') | PatAnn(p', _) -> unwrap_var p'
        | _ -> None
      in
      let vpat = match unwrap_var pat with Some v -> v | None -> failwith "impossible" in
      trans_expr ~table ~var_table rhs (Complex (fun rhs_value ->
          match rhs_value with
          | LVal { lv_var; lv_path = []; lv_src = _ } when vpat.vpat_mut = Imm ->
              let new_var_table = (vpat.vpat_name, { name = lv_var; var_typ = vpat.vpat_typ }) :: var_table in
              apply_stmt_cont cont ~var_table:new_var_table
          | _ ->
              let anf_var = ANF.gen_var () in
              let new_var_table = (vpat.vpat_name, { name = anf_var; var_typ = vpat.vpat_typ }) :: var_table in
              Stmt ( stmt.span, Decl (anf_var, Val rhs_value),
                apply_stmt_cont cont ~var_table:new_var_table)))
  | StmtDeclNoInit pat ->
      (* Declaration without initializer: extract the variable pattern, unwrapping PatMut/PatAnn *)
      let rec unwrap_var (p : Syntax.ParseTree.pattern) : Syntax.ParseTree.var_pattern option =
        match p.shape with
        | PatVar vpat -> Some vpat
        | PatMut(p') | PatAnn(p', _) -> unwrap_var p'
        | _ -> None
      in
      (match unwrap_var pat with
       | Some vpat ->
           let anf_var = ANF.gen_var () in
           let new_var_table = (vpat.vpat_name, { name = anf_var; var_typ = vpat.vpat_typ }) :: var_table in
           Stmt (stmt.span, Decl(anf_var, Val(Int 0)),
             apply_stmt_cont cont ~var_table:new_var_table)
       | None ->
           failwith "uninitialized declaration with non-variable pattern not yet supported in ANF")
  | StmtIf (cond, conseq, alter) ->
      trans_expr ~table ~var_table cond (Complex (fun cond_value ->
          let[@inline] trans_if (k : stmt_continuation) : ANF.program =
            let conseq = trans_stmt ~table ~var_table ~labels ~return conseq k in
            let alter =
              match alter with
              | Some alter -> trans_stmt ~table ~var_table ~labels ~return alter k
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

        loop #cont_loop {
          block #break_loop {
            rest;
          }
          in
          if cond {
            body;
            jump #cont_loop;
          } else {
            jump #break_loop;
          }
        }
      *)
      let label_cont = ANF.gen_label () in
      let label_break = ANF.gen_label () in
      let loop_body =
        trans_expr ~table ~var_table cond (Complex (fun cond_value ->
          Branch {
            br_src = stmt.span;
            br_matched = cond_value;
            br_branches =
              [ (1, trans_stmt ~table ~var_table ~labels:(label_break::label_cont::labels) ~return body (Simple (stmt.span, label_cont))) ];
            br_default = Some ( Jump(stmt.span, label_break, []));
          }))
      in
      let loop_body_with_break =
        ANF.Block
          ( { blk_label = label_break
            ; blk_params = []
            ; blk_body = (apply_stmt_cont cont ~var_table) }
          , loop_body )
      in
      Loop { blk_label = label_cont; blk_params = []; blk_body = loop_body_with_break }
        (* (apply_stmt_cont cont ~var_table) *)
  | StmtMatch (head, arms) ->
      let trans_match k =
        let rec bindings_of_pat acc (pat : Syntax.ParseTree.pattern) =
          match pat.shape with
          | PatWildcard | PatLit _ -> acc
          | PatVar vpat -> vpat.vpat_name :: acc
          | PatAs(pat', vpat) -> bindings_of_pat (vpat.vpat_name :: acc) pat'
          | PatAnn(pat', _) -> bindings_of_pat acc pat'
          | PatMut(pat') -> bindings_of_pat acc pat'
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
              List.map2 (fun name var -> (name, { name = var; var_typ = None })) bindings blk_params
              @ var_table
            in
            (bindings,
              ANF.{
                blk_label = ANF.gen_label ();
                blk_params;
                blk_body = trans_stmt ~table ~var_table ~labels ~return action k;
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
          trans_expr ~table ~var_table head (Complex (fun head_value ->
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
  | StmtBreak -> Jump(stmt.span, List.hd labels, [])
  | StmtContinue -> Jump(stmt.span, List.hd (List.tl labels), [])
  | StmtAssign (op, _left, right) ->
      let lvalue_name = match _left.shape with
        | ExpVar name -> name
        | _ -> failwith "assignment to non-variable lvalue not yet supported in ANF"
      in
      let left_anf_var = lookup_var_name var_table lvalue_name in
      trans_expr ~table ~var_table right (Complex (fun rhs_value ->
          let lv = { ANF.lv_var = left_anf_var; lv_path = []; lv_src = stmt.span } in
          begin match op with
          | None ->
              Stmt(stmt.span, Assign(lv, rhs_value),
                apply_stmt_cont cont ~var_table)
          | Some calc_op ->
              let temp_var = ANF.gen_var () in
              Stmt(stmt.span, Decl(temp_var,
                BinOp(BinOpCalculate calc_op, LVal lv, rhs_value)),
              Stmt(stmt.span, Assign(lv, var_to_value ~src:stmt.span temp_var),
                apply_stmt_cont cont ~var_table))
          end))
  | StmtDecl (pat, rhs) ->
      (* Handle non-trivial patterns: tuple destructuring, struct destructuring *)
      begin match pat.shape with
      | PatVar _ ->
          (* Already handled above, but handle here too for completeness *)
          trans_expr ~table ~var_table rhs (Complex (fun rhs_value ->
              match pat.shape with
              | PatVar vpat ->
                  (match rhs_value with
                   | LVal { lv_var; lv_path = []; _ } when vpat.vpat_mut = Imm ->
                       let new_var_table = (vpat.vpat_name,
                         { name = lv_var; var_typ = vpat.vpat_typ }) :: var_table in
                       apply_stmt_cont cont ~var_table:new_var_table
                   | _ ->
                       let anf_var = ANF.gen_var () in
                       let new_var_table = (vpat.vpat_name,
                         { name = anf_var; var_typ = vpat.vpat_typ }) :: var_table in
                       Stmt(stmt.span, Decl(anf_var, Val rhs_value),
                         apply_stmt_cont cont ~var_table:new_var_table))
              | _ -> failwith "impossible"))
      | _ ->
          (* For complex patterns, use ConvertMatch to decompose *)
          trans_expr ~table ~var_table rhs (Complex (fun rhs_value ->
              bind rhs_value (fun rhs_var ->
                  let rec bindings_of_pat acc (p : Syntax.ParseTree.pattern) =
                    match p.shape with
                    | PatWildcard | PatLit _ -> acc
                    | PatVar vpat -> vpat.vpat_name :: acc
                    | PatAs(p', vpat) -> bindings_of_pat (vpat.vpat_name :: acc) p'
                    | PatAnn(p', _) -> bindings_of_pat acc p'
                    | PatMut(p') -> bindings_of_pat acc p'
                    | PatTuple ps | PatADT(_, ps) ->
                        List.fold_left bindings_of_pat acc ps
                    | PatStruct(_, field_ps) ->
                        List.fold_left (fun acc (_, pp) -> bindings_of_pat acc pp) acc field_ps
                  in
                  let bindings = bindings_of_pat [] pat in
                  let blk_params = List.map (fun _ -> ANF.gen_var ()) bindings in
                  let new_var_table =
                    List.map2 (fun name var -> (name, { name = var; var_typ = None })) bindings blk_params
                    @ var_table
                  in
                  let action _bindings =
                    apply_stmt_cont cont ~var_table:new_var_table
                  in
                  ConvertMatch.trans_match ~table rhs_var [(pat, action)])))
      end
  | StmtFor (pat, iterable, _body) ->
      (* For loop: iterate over an iterable by repeatedly calling a method
         This is a simplified version that uses a while-like structure.
         TODO: full iterator protocol support *)
      trans_expr ~table ~var_table iterable (Complex (fun _iter_value ->
          let label_cont = ANF.gen_label () in
          let label_break = ANF.gen_label () in
          (* For now, assume iterable is a range-like value; bind pattern to a temp *)
          let iter_var = ANF.gen_var () in
          let new_var_table =
            match pat.shape with
            | PatVar vpat ->
                (vpat.vpat_name, { name = iter_var; var_typ = vpat.vpat_typ }) :: var_table
            | _ -> var_table
          in
          let loop_body_with_break =
            ANF.Block
              ( { blk_label = label_break
                ; blk_params = []
                ; blk_body = apply_stmt_cont cont ~var_table:new_var_table }
              , Jump(stmt.span, label_cont, []) )
          in
          Loop { blk_label = label_cont; blk_params = []; blk_body = loop_body_with_break }))

  | StmtCStyleFor (init_opt, cond_opt, incr_opt, body) ->
      (* Desugared as: init; while(cond) { body; incr; } *)
      let build_loop var_table =
        let label_cont = ANF.gen_label () in
        let label_break = ANF.gen_label () in
        (* After executing the body, evaluate incr (if any) then jump to loop condition *)
        let body_continuation =
          Complex (fun var_table ->
            match incr_opt with
            | Some incr ->
                trans_expr ~table ~var_table incr (Complex (fun _ ->
                  ANF.Jump(stmt.span, label_cont, [])))
            | None -> ANF.Jump(stmt.span, label_cont, []))
        in
        let body_prog =
          trans_stmt ~table ~var_table ~labels:(label_break::label_cont::labels) ~return body body_continuation
        in
        (* Condition check: if true, run body; else jump to break *)
        let loop_check =
          match cond_opt with
          | Some cond ->
              trans_expr ~table ~var_table cond (Complex (fun cond_value ->
                ANF.Branch {
                  br_src = stmt.span;
                  br_matched = cond_value;
                  br_branches = [ (1, body_prog) ];
                  br_default = Some (ANF.Jump(stmt.span, label_break, []));
                }))
          | None ->
              body_prog
        in
        (* Break block contains the continuation after the for loop *)
        let loop_body_with_break =
          ANF.Block
            ( { blk_label = label_break
              ; blk_params = []
              ; blk_body = apply_stmt_cont cont ~var_table }
            , loop_check )
        in
        ANF.Loop { blk_label = label_cont; blk_params = []; blk_body = loop_body_with_break }
      in
      (* Run init (if any), then the loop *)
      (match init_opt with
       | Some init ->
           trans_stmt ~table ~var_table ~labels ~return init (Complex (fun var_table ->
             build_loop var_table))
       | None -> build_loop var_table)

and trans_stmts
  ~(table : Semantics.Table.table)
  ~(var_table: var_table)
  ~(labels: ANF.label list)
  ~(return: ANF.label)
  (stmts: Syntax.ParseTree.stmt list)
  (cont: stmt_continuation): ANF.program =
  match stmts with
  | []    -> apply_stmt_cont cont ~var_table
  | s::ss ->
      trans_stmt ~table ~var_table ~labels ~return s (Complex (fun var_table ->
          trans_stmts ~table ~var_table ~labels ~return ss cont))

let trans_func_impl ~(table : Semantics.Table.table)
    (func_impl:func_impl): ANF.function_definition =
  let func_label = ANF.gen_label () in
  let (func_decl, stmt) = func_impl in
  let func_params = List.map (fun _ -> ANF.gen_var ()) func_decl.func_decl_args in
  let var_table =
    List.map2 (fun farg var -> (farg.Syntax.ParseTree.farg_name, { name = var; var_typ = Some farg.farg_typ }))
      func_decl.func_decl_args func_params
  in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params;
      func_label;
      func_body =
        trans_stmt ~table ~var_table ~labels:[] ~return:func_label stmt (Simple (stmt.span, func_label));
    };;

(** Convert a method implementation into an ANF function definition.
    The function name is mangled as "TypeName_methodName". *)
let trans_method_as_func ~(table : Semantics.Table.table)
    (typ_name : Syntax.ParseTree.typ_name)
    ((func_decl, stmt) : Syntax.ParseTree.func_impl) : ANF.function_definition =
  let mangled_name = typ_name ^ "." ^ func_decl.func_decl_name in
  let trans_decl = { func_decl with func_decl_name = mangled_name } in
  trans_func_impl ~table (trans_decl, stmt)

let trans_program ~table (program : top_clause list)
    : ANF.function_definition list =
  let rec go acc = function
    | [] -> List.rev acc
    | { Syntax.ParseTree.shape = FunctionDef func_impl; _ } :: rest ->
        go (trans_func_impl ~table func_impl :: acc) rest
    | { Syntax.ParseTree.shape = MethodsImpl methods_impl; _ } :: rest ->
        let typ_name = methods_impl.impl_typ in
        let funcs = List.rev_map (fun impl ->
          trans_method_as_func ~table typ_name impl)
          methods_impl.impl_methods
        in
        go (List.rev_append funcs acc) rest
    | _ :: rest ->
        go acc rest
  in
  go [] program
