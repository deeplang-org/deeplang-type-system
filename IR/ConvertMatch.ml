
type pattern = Syntax.ParseTree.pattern
type bindings = (string * ANF.variable) list

type action = bindings -> ANF.program

type transl_arm =
  { bindings : bindings
  ; pats     : pattern list
  ; action   : action }

type match_kind =
  | ADT of Syntax.ParseTree.adt_label list
  | Struct of Syntax.ParseTree.struct_field list
  | Lit
  | Tuple of int
  | Unknown

let identify_match (arms : transl_arm list) : match_kind =
  let add_to_list elem lst = if List.mem elem lst then lst else elem :: lst in
  let rec update_kind curr_kind (pat : pattern) =
    match curr_kind, pat.shape with
    | _, (PatWildcard | PatVar _) -> curr_kind
    | _, PatAs (pat', _) -> update_kind curr_kind pat'
    | (Unknown | Lit), PatLit _ -> Lit
    | (Unknown | Tuple _), PatTuple pats -> Tuple (List.length pats)
    | Unknown, PatADT (label, _) -> ADT [ label ]
    | ADT labels, PatADT(label, _) -> ADT (add_to_list label labels)
    | Unknown, PatStruct (_, field_pats) -> Struct (List.map fst field_pats)
    | Struct fields, PatStruct (_, field_pats) ->
        Struct
          (List.fold_left
              (fun fields (field, _) -> add_to_list field fields)
              fields field_pats)
    | _ -> failwith "impossible"
  in
  List.fold_left
    (fun curr_kind { pats; _ } -> update_kind curr_kind (List.hd pats))
    Unknown arms


let dummy_span = Syntax.SyntaxError.dummy_span
let wildcard_pat : pattern =
  { shape = PatWildcard; pat_id = Obj.magic 0; span = dummy_span }


let rec bind_pat bindings value (pat : pattern) =
  match pat.shape with
  | PatVar vpat -> (pat, (vpat.vpat_name, value) :: bindings)
  | PatAs(pat', vpat) -> bind_pat ((vpat.vpat_name, value) :: bindings) value pat'
  | _ -> (pat, bindings)

let rec trans_multi_match ~(table : Semantics.Table.table) (heads : ANF.variable list)
    (arms : transl_arm list) : ANF.program =
  match heads, arms with
  | _, [] -> Abort
  | [], { bindings; action; _ } :: _ -> action bindings
  | [ _ ], { pats = [ { shape = PatWildcard; _ } ]; bindings; action } :: _ ->
      action bindings
  | [ head ], { pats = [ { shape = PatVar vpat; _ } ]; bindings; action } :: _ ->
      action ((vpat.vpat_name, head) :: bindings)
  | head :: heads, arms ->
      match identify_match arms with
      | Unknown ->
          (* all patterns are wildcard or variable *)
          let arms' =
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
          let new_heads = List.init len (fun _ -> ANF.gen_var ()) in
          let new_arms =
            arms |> List.map (fun { bindings; pats; action } ->
              match pats with
              | [] -> failwith "impossible"
              | pat :: pats ->
                  let pat, bindings = bind_pat bindings head pat in
                  let new_pats =
                    match pat.shape with
                    | PatWildcard | PatVar _ -> List.init len (fun _ -> wildcard_pat)
                    | PatTuple pats -> pats
                    | _ -> failwith "impossible"
                  in
                  { bindings; pats = new_pats @ pats; action })
          in
          List.fold_left
            (fun (index, body) new_head ->
                  let lv: ANF.lvalue =
                    { lv_var = head; lv_path = [ Field index ]; lv_src = dummy_span }
                  in
                  (index + 1, ANF.Stmt(dummy_span, Decl(new_head, Val (LVal lv)), body)))
            (0, trans_multi_match ~table (new_heads @ heads) new_arms)
            new_heads
          |> snd
      | ADT labels ->
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
          let arms_of_label =
            labels
            |> List.to_seq
            |> Seq.map (fun label ->
              let label_info = Hashtbl.find table.adt label in
              (label, (label_info, ref [])))
            |> Hashtbl.of_seq
          in
          let default_arms = ref [] in
          let add_to_every_branch bindings (rest : pattern list) action =
            Hashtbl.iter
              (fun _ ((label_info : Semantics.Table.adt_data), arms) ->
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
                    let new_arm = { bindings; pats = pats @ rest; action } in
                    let _, arms = Hashtbl.find arms_of_label label in
                    arms := new_arm :: !arms
                | _ -> failwith "impossible");
          let branches =
            Hashtbl.to_seq arms_of_label
            |> Seq.map (fun (_, ((label_info : Semantics.Table.adt_data), arms)) ->
              let new_heads = List.map (fun _ -> ANF.gen_var ()) label_info.typ in
              let _, body =
                List.fold_left
                  (fun (index, body) new_head ->
                    let lv: ANF.lvalue =
                      { lv_var = head
                      ; lv_path = [ Field index; AsTag label_info.tag ]
                      ; lv_src = dummy_span }
                    in
                    (index + 1, ANF.Stmt(dummy_span, Decl(new_head, Val (LVal lv)), body)))
                  (1, trans_multi_match ~table (new_heads @ heads) (List.rev !arms))
                  new_heads
              in
              ( label_info.tag, body ))
            |> List.of_seq
          in
          let default = trans_multi_match ~table heads !default_arms in
          Branch
            { br_src = dummy_span
            ; br_matched = LVal { lv_var = head; lv_path = []; lv_src = dummy_span }
            ; br_branches = branches
            ; br_default = if is_exhausitive then None else Some default }
      | Lit -> failwith "WIP"
      | Struct _ -> failwith "WIP"


let trans_match ~(table : Semantics.Table.table) (head : ANF.variable)
    (arms : (pattern * (bindings -> ANF.program)) list) : ANF.program =
  trans_multi_match ~table [head]
    (List.map (fun (pat, action) -> { bindings = []; pats = [pat]; action }) arms)
