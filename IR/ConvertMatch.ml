
type pattern = Syntax.ParseTree.pattern

(* pattern matching may bind new variables, and we must keep track of them. *)
type bindings = (string * ANF.variable) list
(* the action of a [match] expression or statement is translated to a ANF program.
   But it need to know what the variables in the pattern are bind to,
   so it accepts an extra [bindings] argument. *)
type action = bindings -> ANF.program


(* The pattern matching algorithm works by translating a match on a *list* of values:

     match v1, v2, ..., vN {
       pat_11, pat_12, ..., pat_1N => action_1
       ..
       pat_M1, pat_M2, ..., pat_MN => action_M
     }

   So patterns and actions in a match forms a matrix.
   A [transl_arm] is one row in this matrix: [pat_i1, pat_i2, ..., pat_iN => action_i].
   In addition to the list of patterns and the final action,
   [transl_arm] records the list of bindings already processed. *)
type transl_arm =
  { bindings : bindings
  ; pats     : pattern list
  ; action   : action }

(* we need to know what kind of value we are matching on.
   One way is to use type information, here we choose an alternative way:
   we identify what we are matching on by inspecting the patterns
   (assuming that the input program is well-typed).

   The benefit of identifying a match by inspecting patterns is,
   we can avoid generating unnecessary branches in the result.
   For example, if [T] is a enum with 100 constructors,
   but only two constructors are mentioned in the patterns,
   then we only need two branches in the result,
   and all other constructors go to the fallback case.

   Similarly, for records, we only extract the fields that are needed. *)
type match_kind =
  (* We are matching on an ADT. The list of labels that are mentioned is recorded *)
  | ADT of Syntax.ParseTree.adt_label list
  (* We are matching on a record.
     The type name of the struct, and the list of fields that are mentioned is recorded *)
  | Struct of Syntax.ParseTree.typ_name * Syntax.ParseTree.struct_field list
  (* We are matching on a primitive type.
     The list of literal values that are mentioned is recorded *)
  | Lit of Syntax.ParseTree.literal list
  (* [Tuple n]: We are matching on a tuple of length [n] *)
  | Tuple of int
  (* Don't know what we are matching on.
     In this case, there can be only variable and wildcard patterns.
     So any value will match. *)
  | Unknown

(* identify what kind of value is matched by the first column of a pattern matrix *)
let identify_match (arms : transl_arm list) : match_kind =
  let add_to_list elem lst = if List.mem elem lst then lst else elem :: lst in
  let rec update_kind curr_kind (pat : pattern) =
    match curr_kind, pat.shape with
    | _, (PatWildcard | PatVar _) -> curr_kind
    | _, PatAs (pat', _) -> update_kind curr_kind pat'
    | Unknown, PatLit lit -> Lit [ lit ]
    | Lit lits, PatLit lit -> Lit (add_to_list lit lits)
    | (Unknown | Tuple _), PatTuple pats -> Tuple (List.length pats)
    | Unknown, PatADT (label, _) -> ADT [ label ]
    | ADT labels, PatADT(label, _) -> ADT (add_to_list label labels)
    | Unknown, PatStruct (tname, field_pats) -> Struct (tname, List.map fst field_pats)
    | Struct (tname, fields), PatStruct (_, field_pats) ->
        let fields =
          List.fold_left
            (fun fields (field, _) -> add_to_list field fields)
            fields field_pats
        in
        Struct (tname, fields)
    | _ -> failwith "impossible"
  in
  List.fold_left
    (fun curr_kind { pats; _ } -> update_kind curr_kind (List.hd pats))
    Unknown arms


let trans_lit (lit : Syntax.ParseTree.literal) : ANF.value =
  match lit with
  | LitUnit       -> Int 0
  | LitBool true  -> Int 1
  | LitBool false -> Int 0
  | LitInt i      -> Int i
  | LitFloat f    -> Float f
  | LitChar ch    -> Int ch
  | LitString str -> String str


let dummy_span = Syntax.SyntaxError.dummy_span
let wildcard_pat : pattern =
  { shape = PatWildcard; pat_id = Obj.magic 0; span = dummy_span }


(* assume we are [value] matches [pat],
   then [bind_pat bindings value pat] deref all [as] patterns in [pat],
   bind variables in [pat] to [value],
   returning the deferenced pattern and the list of new bindings *)
let rec bind_pat bindings value (pat : pattern) =
  match pat.shape with
  | PatVar vpat -> (pat, (vpat.vpat_name, value) :: bindings)
  | PatAs(pat', vpat) -> bind_pat ((vpat.vpat_name, value) :: bindings) value pat'
  | _ -> (pat, bindings)

(* [pat_is_irrefutable pat = true] means [pat] matches any value *)
let rec pat_is_irrefutable (pat : pattern) =
  match pat.shape with
  | PatVar _ | PatWildcard -> true
  | PatAs (pat', _) -> pat_is_irrefutable pat'
  | _ -> false

(* [trans_multi_match ~table heads arms] translate a match on a list of values [heads],
   against a pattern matrix [arms], and produce a program. *)
let rec trans_multi_match ~(table : Semantics.Table.table) (heads : ANF.variable list)
    (arms : transl_arm list) : ANF.program =
  match heads, arms with
  | _, [] ->
      (* there is now row in the match matrix,
         so this is a match failure *)
      Abort
  | [], { bindings; action; _ } :: _ ->
      (* All values are matched. So the first remaining row will be executed *)
      action bindings
  | _, { pats; bindings; action } :: _ when List.for_all pat_is_irrefutable pats ->
      (* All patterns in the first row is irrefutable,
         so any list of value will match the first row,
         and the action of the first row will be executed. *)
      let bindings =
        List.fold_left2
          (fun bindings head pat -> snd (bind_pat bindings head pat)) bindings heads pats
      in
      action bindings
  | head :: heads, arms ->
      match identify_match arms with
      | Unknown ->
          (* all patterns in the first column are wildcard or variable.
             In this case, the first value is irrelevant, and we simply skip it. *)
          let arms' =
            (* remove the first pattern from each row.
               Note that although all these patterns are irrefutable,
               they may still create new bindings. *)
            List.map
              (fun arm ->
                  match arm.pats with
                  | [] -> failwith "impossible"
                  | pat :: pats ->
                      let _, bindings = bind_pat arm.bindings head pat in
                      { arm with pats; bindings })
              arms
          in
          trans_multi_match ~table heads arms'
      | Tuple len ->
          (* destruct the tuple, extract its components,
             and continue matching all its components. *)
          let new_heads = List.init len (fun _ -> ANF.gen_var ()) in
          let new_arms =
            arms |> List.map (fun { bindings; pats; action } ->
              match pats with
              | [] -> failwith "impossible"
              | pat :: pats ->
                  let pat, bindings = bind_pat bindings head pat in
                  let new_pats =
                    match pat.shape with
                    | PatWildcard | PatVar _ ->
                        (* we are matching a tuple [(v1, ..., vN)] against wildcard or variable,
                           so the components [v1, ..., vN] can be anything and will always match. *)
                        List.init len (fun _ -> wildcard_pat)
                    | PatTuple pats ->
                        (* we are matching a tuple [(v1, ..., vN)] against [(p1, ..., pN)],
                           so every [vi] should be matched against [pi]. *)
                        pats
                    | _ -> failwith "impossible"
                  in
                  { bindings; pats = new_pats @ pats; action })
          in
          List.fold_left
            (fun (index, body) new_head ->
                  (* destruct the tuple by generating [let xi = head.i] *)
                  let lv: ANF.lvalue =
                    { lv_var = head; lv_path = [ Field index ]; lv_src = dummy_span }
                  in
                  (index + 1, ANF.Stmt(dummy_span, Decl(new_head, Val (LVal lv)), body)))
            (0, trans_multi_match ~table (new_heads @ heads) new_arms)
            new_heads
          |> snd
      | ADT labels ->
          (* if the match is exhausitive, there is no need to create a [default] branch *)
          let is_exhausitive =
            match labels with
            | [] -> false
            | label0 :: _ ->
                let label_info = Hashtbl.find table.adt label0 in
                match Hashtbl.find table.typ label_info.sum with
                | ADT_data { core; _ } ->
                    List.for_all (fun (label, _) -> List.mem label labels) core
                | _ -> failwith "impossible"
          in
          (* we are matching on an ADT, so the generated code will be of the shape:

               branch head.label {
                 Label_1 => ...
                 Label_2 => ...
                 ...
                 Label_N => ...
                 // optionally
                 _ => ...
               }

             We only need to generate branches for those mentioned labels in [labels].

             The challege here is to fill the [...].
             For every [Label_i], we need to know that if [head] is [Label_i(...)], what remains to be matched.
             We do this by generating a sub-problem (a new match matrix) for every label and the default branch. *)

          (* A hashtable from [label] to the sub-problem remaining if [head] is [label] *)
          let arms_of_label =
            labels
            |> List.to_seq
            |> Seq.map (fun label ->
              let label_info = Hashtbl.find table.adt label in
              (label, (label_info, ref [])))
            |> Hashtbl.of_seq
          in
          (* the sub-problem remaining is [head] is none of [labels] *)
          let default_arms = ref [] in
          (* For wildcard/variable patterns, no matter what the label of [head] is,
             they will match [head] succesfully. So they will appear in every sub-problem.
             In this case, we call [add_to_every_branch] on them. *)
          let add_to_every_branch bindings (rest : pattern list) action =
            Hashtbl.iter
              (fun _ ((label_info : Semantics.Table.adt_data), arms) ->
                (* Note the arity of the new arm here.
                   If [head] is [label], then we must also match the payload of [label].
                   So we extract the arity of [label] from its type information. *)
                let new_arm =
                  { bindings
                  ; action
                  ; pats = List.map (fun _ -> wildcard_pat) label_info.typ @ rest }
                in
                arms := new_arm :: !arms)
              arms_of_label;
            if not is_exhausitive then
              default_arms := { bindings; action; pats = rest } :: !default_arms
          in
          arms |> List.iter (fun { bindings; pats; action } ->
            match pats with
            | [] -> failwith "impossible"
            | pat :: rest ->
                let pat, bindings = bind_pat bindings head pat in
                match pat.shape with
                | PatWildcard | PatVar _ -> add_to_every_branch bindings rest action
                | PatADT (label, pats) ->
                    (* continue matching [label]'s payloads *)
                    let new_arm = { bindings; pats = pats @ rest; action } in
                    let _, arms = Hashtbl.find arms_of_label label in
                    arms := new_arm :: !arms
                | _ -> failwith "impossible");
          (* We have computed the sub-problem associated with each branch.
             Now we recursively translate the sub-problems and generate a [ANF.branch] *)
          let branches =
            Hashtbl.to_seq arms_of_label
            |> Seq.map (fun (_, ((label_info : Semantics.Table.adt_data), arms)) ->
              (* generate fresh variables that bind the payloads of a label *)
              let new_heads = List.map (fun _ -> ANF.gen_var ()) label_info.typ in
              let _, body =
                List.fold_left
                  (fun (index, body) new_head ->
                    (* bind the payload of labels to the new variables *)
                    let lv: ANF.lvalue =
                      { lv_var = head
                      ; lv_path = [ Field index; AsTag label_info.tag ]
                      ; lv_src = dummy_span }
                    in
                    (index + 1, ANF.Stmt(dummy_span, Decl(new_head, Val (LVal lv)), body)))
                  (0, trans_multi_match ~table (new_heads @ heads) (List.rev !arms))
                  new_heads
              in
              ( label_info.tag, body ))
            |> List.of_seq
          in
          Branch
            { br_src = dummy_span
            ; br_matched = LVal { lv_var = head; lv_path = [ Tag ]; lv_src = dummy_span }
            ; br_branches = branches
            ; br_default =
                if is_exhausitive
                then None
                else Some (trans_multi_match ~table heads !default_arms) }
      | Struct (type_name, used_fields) ->
          (* matching a struct is similar to matchig a tuple,
             except that we only extract the mentioned fields. *)
          let new_heads = List.map (fun _ -> ANF.gen_var ()) used_fields in
          let new_arms =
            arms |> List.map (fun { bindings; pats; action } ->
              match pats with
              | [] -> failwith "impossible"
              | pat :: pats ->
                  let pat, bindings = bind_pat bindings head pat in
                  let new_pats =
                    match pat.shape with
                    | PatWildcard | PatVar _ -> List.map (fun _ -> wildcard_pat) new_heads
                    | PatStruct (_, field_pats) ->
                        used_fields |> List.map (fun field ->
                          match List.assoc field field_pats with
                          | pat -> pat
                          | exception Not_found -> wildcard_pat)
                    | _ -> failwith "impossible"
                  in
                  { bindings; pats = new_pats @ pats; action })
          in
          let[@warning "-partial-match"] (Semantics.Table.Struct_data { core; _ }) =
            Hashtbl.find table.typ type_name
          in
          List.fold_left2
            (fun body new_head label ->
                let index = (Hashtbl.find core label).index in
                let lv: ANF.lvalue =
                  { lv_var = head; lv_path = [ Field index ]; lv_src = dummy_span }
                in
                ANF.Stmt(dummy_span, Decl(new_head, Val(LVal lv)), body))
            (trans_multi_match ~table (new_heads @ heads) new_arms)
            new_heads used_fields
      | Lit lits ->
          (* match on some literals, such as float or string, cannot be compiled to switch.
             So for literals, we generate nested [if-then-else] ad equality test:

               match x {
                 lit_1 => action_1
                 ...
                 lit_N => action_N
                 _ => default_action
               }

             is translated to

               if (x == lit_1) {
                action_1
               }
               ...
               else if (x == lit_N) {
                 action_N
               }
               else {
                 default_action
               }

             TODO: generate switch for unit, boolean and dense integer matches. *)
          let is_exhausitive =
            match lits with
            | [ LitBool true; LitBool false ] | [ LitBool false; LitBool true ] -> true
            | [ LitUnit ] -> true
            | _ -> false
          in
          (* Similar to the case of ADT,
             we compute a sub-problem for every mentioned literal [l],
             which represents what remains to be done if [head == l]. *)
          let arms_of_lit =
            List.to_seq lits
            |> Seq.map (fun lit -> (lit, ref []))
            |> Hashtbl.of_seq
          in
          let default_arms = ref [] in
          (* literals have no payload, so no new heads will get generated *)
          let add_to_every_branch bindings (rest : pattern list) action =
            let new_arm = { bindings; action; pats = rest } in
            Hashtbl.iter (fun _ arms -> arms := new_arm :: !arms) arms_of_lit;
            if not is_exhausitive then
              default_arms := new_arm :: !default_arms
          in
          arms |> List.iter (fun { bindings; pats; action } ->
            match pats with
            | [] -> failwith "impossible"
            | pat :: rest ->
                let pat, bindings = bind_pat bindings head pat in
                match pat.shape with
                | PatWildcard | PatVar _ -> add_to_every_branch bindings rest action
                | PatLit lit ->
                    let new_arm = { bindings; pats = rest; action } in
                    let arms = Hashtbl.find arms_of_lit lit in
                    arms := new_arm :: !arms
                | _ -> failwith "impossible");
          let head_value : ANF.value =
            LVal { lv_var = head; lv_path = []; lv_src = dummy_span }
          in
          (* generate iterated if-then-else and equality test *)
          List.fold_left
            (fun body lit ->
                let arms = Hashtbl.find arms_of_lit lit in
                let result_var = ANF.gen_var () in
                ANF.Stmt(
                  dummy_span,
                  Decl (result_var, BinOp(BinOpCompare BinOpEq, head_value, trans_lit lit)),
                  Branch
                    { br_src = dummy_span
                    ; br_matched =
                        LVal { lv_var = result_var; lv_path = []; lv_src = dummy_span }
                    ; br_branches = [ 1, trans_multi_match ~table heads (List.rev !arms) ]
                    ; br_default = Some body }))
            (trans_multi_match ~table heads !default_arms)
            lits


let trans_match ~(table : Semantics.Table.table) (head : ANF.variable)
    (arms : (pattern * (bindings -> ANF.program)) list) : ANF.program =
  trans_multi_match ~table [head]
    (List.map (fun (pat, action) -> { bindings = []; pats = [pat]; action }) arms)
