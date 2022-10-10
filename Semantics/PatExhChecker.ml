(** Pattern Exhausitive Checker *)

open Syntax.ParseTree;;
open Walker;;
open Context;;
type space =
  (* unfold lazily *)
  | Fold      of typ_shape
  (* the following are builtin types *)
  | Unit
  | BoolTrue
  | BoolFalse
  | Builtin   of typ_shape
  | Tuple     of space list (* s1*s2*...*sn *)
  (* different branches introduced by ADT Constructors *)
  | Branches  of space list (* s1|s2|...|sn *)
  | ADT_Label of adt_label * space list  
    (* Constructor(s1, s2, ... , sn) *)
  | Struct    of typ_name * (struct_field * space) list
    (* Struct { field=space } *)
  ;;

let ty_space (ty:typ) : space = Fold ty.shape;;
(** ty is the type of the pattern *)
let rec pat_space (context:context) (pat:pattern) (typ:typ) : space = match pat.shape with
  | PatWildcard -> ty_space typ
  | PatLit(literal) -> ( match literal with
    | LitUnit      -> Unit
    | LitBool(true)   -> BoolTrue
    | LitBool(false)  -> BoolFalse
    | LitInt(_)    -> ty_space Helper.i32  (* Discuss *)
    | LitFloat(_)  -> ty_space Helper.f32  (* Discuss *)
    | LitChar(_)   -> ty_space Helper.char 
    | LitString(s) -> ty_space (Helper.array Helper.char (String.length s + 1)) (* Discuss *)
  )
  (* here, maybe some variables will be covered
      Cons1(x, Cons2(x, y)), the `x` should be the `x` in Cons2
   *)
  | PatVar(vpat) -> 
    ( match vpat.vpat_typ with
    | None -> add_variable context vpat.vpat_symb vpat.vpat_mut vpat.vpat_name (Some typ)
    | Some(ty) -> 
        if Helper.ty_eq ty typ then 
            add_variable context vpat.vpat_symb vpat.vpat_mut vpat.vpat_name vpat.vpat_typ
        else
            failwith("declared type doesn't match with the given pattern/expr")
    );
    ty_space typ
  | PatAs(pattern, vpat) -> 
    let _ = pat_space context pattern typ in (* adding variables *)
    ( match vpat.vpat_typ with
    | None -> add_variable context vpat.vpat_symb vpat.vpat_mut vpat.vpat_name (Some typ)
    | Some(ty) -> 
        if Helper.ty_eq ty typ then 
            add_variable context vpat.vpat_symb vpat.vpat_mut vpat.vpat_name vpat.vpat_typ
        else
            failwith("declared type doesn't match with the given pattern/expr")
    );
    ty_space typ
  | PatTuple(patterns) -> 
    let typs = ( match typ.shape with
    | TyTuple(typs) -> typs
    | _ -> failwith("pattern/expr is NOT a Tuple")
    ) in
    Tuple(List.map2 (pat_space context) patterns typs)
  | PatADT(adt_label, patterns) -> 
    let typs = 
    ( match Hashtbl.find_opt context.table.adt adt_label with
    | Some(data) -> data.typ
    | None -> failwith("adt_lable" ^adt_label ^ "not found")
    ) in
    (* WARNING : raise error if length of patterns and that of adt children differ *)
    ADT_Label(adt_label, (List.map2 (pat_space context) patterns typs))
  | PatStruct(type_name, _) ->  (* field_patterns *)
    let _ = ( match Hashtbl.find_opt context.table.typ type_name with
    | Some(Struct_data(data)) -> data.core
    | _ -> failwith("name "^type_name^" is not a struct")
    ) in
    (* TODO : delegate *)
    Fold typ.shape
(* 
let ty_unfold (context:context) (ty:typ) : space = match ty.shape with
  | _ -> 
  ;;
*)