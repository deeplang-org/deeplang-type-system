(** Struct Delegation Resolver *)

open Syntax.ParseTree;;
open Context;;

let def_check (context:context) (def:struct_def) : unit =
  let table = context.table in
  let name = def.struct_name in 
  ( match Hashtbl.find_opt table.typ name with
  | Some(_) -> failwith("The same Struct Name")
  | None -> ()
  );

  (* check whether there exists duplicate field names *)
  let checked_add core field_name field_data = 
    match Hashtbl.find_opt core field_name with
    | Some(_) -> failwith(" there is field named with " ^ field_name)
    | None -> Hashtbl.add core field_name field_data
  in

  let core = Hashtbl.create 10 in
  let fields = def.struct_fields in
  let walk_iter ((field_name, typ, ast_attr):struct_def_field) = (
    let _ = TypExistChecker.check context typ in
    (* for those normal fields *)
    checked_add core field_name {typ=typ; attr=JustField};

    (* for those delegated fields *)
    match ast_attr with
    | Struct_Field -> ()
    | Struct_Delegate -> (
      match typ.shape with 
      | TyNamed(typ_name, _) -> 
        let sub_core : (struct_field, field_data) Hashtbl.t = 
        ( match Hashtbl.find_opt table.typ typ_name with
        | Some(Struct_data(data)) -> data.core
        | _ -> failwith(" typ is not a struct type")
        ) in
        let walk_iter field_name (data:field_data) =
            checked_add core field_name {typ=data.typ; attr=Delegation(typ_name)}
        in Hashtbl.iter walk_iter sub_core
      | _ -> failwith(" typ is not a struct type")
    )
  ) in
  List.iter walk_iter fields;
  Hashtbl.add table.typ name 
      ( Struct_data(
      { intf = []
      ; meth = Hashtbl.create 10
      ; core = core
      ; fields = fields
      }))
;;

(** implict delegation will be converted to explicit field projection *)
(*  TODO
let pat_convert (context:context) (typ_name, field_patterns) 
  : (typ_name * (struct_field * pattern) list) =
  let table = context.table in
  let core = match Hashtbl.find_opt table.typ typ_name with
    | Some(Struct_data(data)) -> data
    | _ -> failwith(" type " ^ typ_name ^ " is not a Struct")
  in
  let iter (field, pat) = match Hashtbl.find_opt core field with
    | None -> failwith(" field " ^ field ^ " not found in type " ^ typ_name)
    | Some(data) -> match data.attr with
      | 
*)