(** Helper *)


open Syntax.ParseTree;;
let rec ty_eq (t1:typ) (t2:typ) : bool = 
    match (t1.shape, t2.shape) with
    | (TyArray(t1', size1), TyArray(t2', size2)) -> 
        if size1=size2 then ty_eq t1' t2' else false
    
    | (TyTuple(ts1), TyTuple(ts2)) -> 
        if List.compare_lengths ts1 ts2 = 0 
        then List.equal ty_eq ts1 ts2
        else false
    | (TyNamed(name1, ts1), TyNamed(name2, ts2)) ->
        if name1=name2 then 
            if List.compare_lengths ts1 ts2 = 0 
            then List.equal ty_eq ts1 ts2
            else false
        else false
    | _ -> t1.shape=t2.shape (* leaf nodes *)
    ;;
let span_dummy : Syntax.SyntaxError.src_span = 
    { span_start= Lexing.dummy_pos
    ; span_end  = Lexing.dummy_pos
    };;
let mk_typ (shape:typ_shape) : typ = 
    { shape = shape
    ; span  = span_dummy
    }

let unit = mk_typ TyUnit ;;
let bool = mk_typ TyBool ;;
let i8 = mk_typ (TyInt(Signed, ISize_8)) ;;
let i16 = mk_typ (TyInt(Signed, ISize_16)) ;;
let i32 = mk_typ (TyInt(Signed, ISize_32)) ;;
let i64 = mk_typ (TyInt(Signed, ISize_64)) ;;
let u8 = mk_typ (TyInt(Unsigned, ISize_8)) ;;
let u16 = mk_typ (TyInt(Unsigned, ISize_16)) ;;
let u32 = mk_typ (TyInt(Unsigned, ISize_32)) ;;
let u64 = mk_typ (TyInt(Unsigned, ISize_64)) ;;
let f32 = mk_typ (TyFloat(FSize_32)) ;;
let f64 = mk_typ (TyFloat(FSize_64)) ;;
let char = mk_typ TyChar ;;
let array (typ:typ) (size:int) = mk_typ (TyArray(typ, size)) ;;
let tuple (typs:typ list) = mk_typ (TyTuple(typs)) ;;
let named (name:typ_name) (tys:typ list) = mk_typ (TyNamed(name, tys)) ;;
let var (name:typ_name) = mk_typ (TyVar(name)) ;;
let this = mk_typ TyThis ;;

let rec pp_ty fmt (ty:typ) =
    let open Format in 
    ( match ty.shape with
    | TyThis -> fprintf fmt  "This" 
    | TyUnit -> fprintf fmt  "()" 
    | TyBool -> fprintf fmt "Bool" 
    | TyChar -> fprintf fmt  "Char" 
    | TyInt(Signed, ISize_8) -> fprintf fmt "I8"
    | TyInt(Signed, ISize_16) -> fprintf fmt "I16" 
    | TyInt(Signed, ISize_32) -> fprintf fmt "I32" 
    | TyInt(Signed, ISize_64) -> fprintf fmt "I64" 
    | TyInt(Unsigned, ISize_8) -> fprintf fmt "U8"
    | TyInt(Unsigned, ISize_16) -> fprintf fmt "U16" 
    | TyInt(Unsigned, ISize_32) -> fprintf fmt "U32" 
    | TyInt(Unsigned, ISize_64) -> fprintf fmt "U64" 
    | TyFloat(FSize_32) -> fprintf fmt "F32"
    | TyFloat(FSize_64) -> fprintf fmt "F64"
    | TyArray(ty, size) -> fprintf fmt "[%a ; %d]" pp_ty ty size
    | TyTuple(tys) -> pp_tys fmt tys
    | TyNamed(name, tys) -> 
        ( match tys with 
        | [] -> fprintf fmt "%s" name
        | _::_ -> fprintf fmt "%s%a" name pp_tys tys
        ) 
    | TyVar(name) -> fprintf fmt "%s@," name
    )
and 
pp_tys fmt (tys:typ list) = 
    let open Format in 
    fprintf fmt "@[<4>(";
    let final = (List.length tys) - 1 in
    let iteri idx ty = 
        pp_ty fmt ty ;
        fprintf fmt (if idx = final then ")@]" else ",@,@ ")
    in 
    List.iteri iteri tys
    

let print_ty = pp_ty Format.std_formatter;;

open Context;;

let pp_var_entry fmt (Symbol(symbol)) (data:var_data) = 
    let mut =
    ( match data.mut with
    | Imm -> "imm"
    | Mut -> "mut"
    ) in
    Format.fprintf fmt 
    "%s, %d : %s %a\n" 
    data.name symbol mut pp_ty data.typ
    ;;
let pp_var_table fmt (table:var_table) =
    Format.fprintf fmt "@[<4>{\n%a}@]"
    (fun fmt -> Hashtbl.iter (pp_var_entry fmt)) table
    ;;

let pp_fun_entry fmt name (data:fun_data) = 
    Format.fprintf fmt
    "%s : %a -> %a\n" name pp_tys data.args pp_ty data.rety
let pp_fun_table fmt (table:fun_table) = 
    Format.fprintf fmt "@[<4>{\n%a}@]"
    (fun fmt -> Hashtbl.iter (pp_fun_entry fmt)) table
    ;;
let pp_adty_data fmt (data:adty_data) =
    Format.fprintf fmt "@[<4>{intf:%a@,meth:%a@,core : %a}@]"
    (fun fmt -> List.iter (Format.fprintf fmt "%s@ ")) data.intf
    pp_fun_table data.meth
    (fun fmt -> List.iter (fun (label, tys) 
        -> Format.fprintf fmt "%s%a" label pp_tys tys)) data.core
    ;;

let pp_struct_data fmt (data:struct_data) = 
    Format.fprintf fmt "@[<4>{intf:%a@,meth:%a@,core:@[<4>{\n%a}@]}@]"
    (fun fmt -> List.iter (Format.fprintf fmt "%s@ ")) data.intf
    pp_fun_table data.meth
    (fun fmt core -> 
        let iter (name:struct_field) (data:field_data) = 
            Format.fprintf fmt ".%s : %a\n"
            name pp_ty data.typ 
        in Hashtbl.iter iter core
    ) data.core
    ;;

let pp_intf_data fmt (data:intf_data) =
    Format.fprintf fmt "@[<4>{meth:%a@}@]"
    pp_fun_table data.meth
    ;;
    
let pp_typ_entry fmt name = function
    | ADT_data(data) -> Format.fprintf fmt "%s ADT : %a " name pp_adty_data data
    | Struct_data(data) -> Format.fprintf fmt "%s Struct : %a " name pp_struct_data data
    | Intf_data(data) -> Format.fprintf fmt "%s Intf : %a " name pp_intf_data data
    ;;

let pp_typ_table fmt (table:typ_table) =
    Format.fprintf fmt "@[<4>{\n%a}@]"
    (fun fmt -> Hashtbl.iter (pp_typ_entry fmt)) table
    ;;

let pp_adt_entry fmt label (data:adt_data) =
    Format.fprintf fmt "%s : %a -> %a\n"
    label pp_tys data.typ pp_ty (named data.sum [])
    ;;

let pp_adt_table fmt (table:adt_table) =
    Format.fprintf fmt "@[<4>{\n%a}@]"
    (fun fmt -> Hashtbl.iter (pp_adt_entry fmt)) table
    ;;

let pp_ref_entry fmt (NodeId.ExprId(id)) (data:ref_data) =
    match data.sym with Symbol(symbol)->
    Format.fprintf fmt "%d : { sym = %d }\n" id symbol
    ;;

let pp_ref_table fmt (table:ref_table) =
    Format.fprintf fmt "@[<4>{\n%a}@]"
    (fun fmt -> Hashtbl.iter (pp_ref_entry fmt)) table
    ;;
