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

open Table;;

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

let pp_literal fmt = function
    | LitUnit -> Format.fprintf fmt "()"
    | LitBool(b) -> Format.fprintf fmt "%b" b
    | LitInt(i) -> Format.fprintf fmt "%d" i
    | LitFloat(f) -> Format.fprintf fmt "%f" f
    | LitChar(c) -> Format.fprintf fmt "%c" (Char.chr c)
    | LitString(s) -> Format.fprintf fmt "%s" s
    ;;

let pp_var_pattern fmt (vpat:var_pattern) =
    let mutability = (
        match vpat.vpat_mut with
        | Imm -> ""
        | Mut -> "mut "
    ) in
    Format.fprintf fmt "%s%s" mutability vpat.vpat_name;
    match vpat.vpat_typ with
    | None -> ()
    | Some(ty) -> Format.fprintf fmt " : %a" pp_ty ty
    ;;
let rec pp_pattern fmt (pattern:pattern) : unit = 
    match pattern.shape with
    | PatWildcard -> Format.fprintf fmt "_"
    | PatLit(literal) -> pp_literal fmt literal
    | PatVar(vpat) -> pp_var_pattern fmt vpat
    | PatAs(_) -> Format.fprintf fmt "TODO"
    (* | PatADT(adt_label, patterns) ->  *)
    | _ -> ()
and pp_patterns fmt (patterns : pattern list) : unit = 
    let final = (List.length patterns) - 1 in
    let iteri idx pattern = 
        pp_pattern fmt pattern ;
        Format.fprintf fmt (if idx = final then "" else ",@ ")
    in List.iteri iteri patterns
    ;;

let pp_ty fmt (typ:typ) : unit = 
    match typ.shape with
    | TyVar(name) -> Format.fprintf fmt "%s" name
    | TyUnit -> Format.fprintf fmt "unit"
    | TyBool -> Format.fprintf fmt "bool"
    | TyInt(sign, size) -> 
        let sign = 
        ( match sign with
        | Signed -> "i"
        | Unsigned -> "u"
        ) in
        let size = 
        ( match size with
        | ISize_8 -> "8"
        | ISize_16 -> "16"
        | ISize_32 -> "32"
        | ISize_64 -> "64"
        ) in
        Format.fprintf fmt "%s%s" sign size
    | TyFloat(size) -> 
        let size = 
        ( match size with
        | FSize_32 -> "32"
        | FSize_64 -> "64"
        ) in
        Format.fprintf fmt "f%s" size
    | TyChar -> Format.fprintf fmt "char"
    | TyThis -> Format.fprintf fmt "this"
    | TyArray(ty, size) -> Format.fprintf fmt "[%a ; %d]" pp_ty ty size
    | TyTuple(tys) -> pp_tys fmt tys
    | TyNamed(name, tys) -> Format.fprintf fmt "%s{%a}" name pp_tys tys

and pp_tys fmt (tys:typ list) =
    Format.fprintf fmt "@[<4>(";
    let final = (List.length tys) - 1 in
    let iteri idx ty = 
        pp_ty fmt ty ;
        Format.fprintf fmt (if idx = final then ")@]" else ",@,@ ")
    in 
    List.iteri iteri tys
    ;;
let rec pp_expr fmt (expr:expr) : unit = 
    let unop_to_string = function
    | UnOpNot -> "!"
    | UnOpNeg -> "-"
    in
    let binop_to_string = function
    | BinOpCompare(cmp) -> 
        ( match cmp with
        | BinOpLt -> "<"
        | BinOpLeq -> "<="
        | BinOpGt -> ">"
        | BinOpGeq -> ">="
        | BinOpEq -> "=="
        | BinOpNeq -> "!="
        )
    | BinOpCalculate(calc) -> 
        ( match calc with
        | BinOpLOr -> "||"
        | BinOpLAnd -> "&&"
        | BinOpLXor -> "^^"
        | BinOpBOr -> "|"
        | BinOpBAnd -> "&"
        | BinOpBXor -> "^"
        | BinOpLShift -> "<<"
        | BinOpRShift -> ">>"
        | BinOpAdd -> "+"
        | BinOpSub -> "-"
        | BinOpMul -> "*"
        | BinOpDiv -> "/"
        | BinOpMod -> "%"
        )
    in 
    match expr.shape with
    | ExpLit(lit) -> pp_literal fmt lit
    | ExpVar(name) -> Format.fprintf fmt "%s" name
    | ExpUnOp(op, expr) -> Format.fprintf fmt "%s %a" (unop_to_string op) pp_expr expr
    | ExpBinOp(op, expr1, expr2) -> Format.fprintf fmt "%a %s %a" pp_expr expr1 (binop_to_string op) pp_expr expr2
    | ExpTuple(exprs) -> pp_exprs fmt exprs
    | ExpADT(label, exprs) -> Format.fprintf fmt "%s %a" label pp_exprs exprs
    | ExpStruct(name, fields) -> (
        Format.fprintf fmt "%s {" name;
        let final = (List.length fields) - 1 in
        let iteri idx (name, expr) = 
            Format.fprintf fmt "%s = %a" name pp_expr expr;
            Format.fprintf fmt (if idx = final then "}" else ",@,@ ")
        in 
        List.iteri iteri fields
    )
    | ExpField(expr, name) -> Format.fprintf fmt "%a.%s" pp_expr expr name
    | ExpThis -> Format.fprintf fmt "this"
    | ExpApp(func, exprs) -> Format.fprintf fmt "%s(%a)" func pp_exprs exprs
    | ExpMethod(expr, name, exprs) -> Format.fprintf fmt "%a.%s(%a)" pp_expr expr name pp_exprs exprs
    | ExpIf(cond, expr1, expr2) -> Format.fprintf fmt "if %a then %a else %a" pp_expr cond pp_expr expr1 pp_expr expr2
    | _ -> ()
    
and pp_exprs fmt (exprs:expr list) =
    Format.fprintf fmt "@[<4>(";
    let final = (List.length exprs) - 1 in
    let iteri idx expr = 
        pp_expr fmt expr ;
        Format.fprintf fmt (if idx = final then ")@]" else ",@,@ ")
    in
    List.iteri iteri exprs
    ;;

let rec pp_stmt fmt (stmt:stmt) : unit =
    match stmt.shape with
    | StmtSeq(stmts) ->
        let final = (List.length stmts) - 1 in
        let iteri idx stmt = 
            pp_stmt fmt stmt ;
            Format.fprintf fmt (if idx = final then "" else "@,@ ")
        in 
        List.iteri iteri stmts
    | StmtExpr(expr) -> pp_expr fmt expr
    | StmtAssign(calc_op, expr1, expr2) -> (
        let calc_op = 
        ( match calc_op with
        | None -> "="
        | Some(op) -> 
            ( match op with
                | BinOpAdd -> "+="
                | BinOpSub -> "-="
                | BinOpMul -> "*="
                | BinOpDiv -> "/="
                | BinOpMod -> "%="
                | _ -> "="
            )
        ) in
        Format.fprintf fmt "%a %s %a;\n" pp_expr expr1 calc_op pp_expr expr2
    )
    | StmtDecl(pattern, expr) -> Format.fprintf fmt "let %a = %a;\n" pp_pattern pattern pp_expr expr
    | _ -> ()
    ;;
let pp_args fmt (args:func_arg list) : unit = 
    Format.fprintf fmt "@[<4>(";
    let final = (List.length args) - 1 in
    let iteri idx arg = 
        Format.fprintf fmt "%s: %a" arg.farg_name pp_ty arg.farg_typ;
        Format.fprintf fmt (if idx = final then ")@]" else ",@,@ ")
    in 
    List.iteri iteri args
    ;;
let pp_fun fmt ((func_decl, stmt): func_impl) : unit = 
    Format.fprintf fmt "fun %s%a : %a {\n" func_decl.func_decl_name pp_args func_decl.func_decl_args pp_ty func_decl.func_decl_rety;
    pp_stmt fmt stmt;
    Format.fprintf fmt "}\n"

let pp_top fmt (clause:top_clause) : unit = 
    match clause.shape with 
    | GlobalVarDef(gvar) -> (
        match (gvar.gvar_typ) with 
        | Some(typ) -> 
            Format.fprintf fmt "let %s : %a = %a\n" gvar.gvar_name pp_ty typ pp_expr gvar.gvar_value
        | None -> 
            Format.fprintf fmt "let %s = %a\n" gvar.gvar_name pp_expr gvar.gvar_value
    )

    | StructDef(def) -> 
        Format.fprintf fmt "struct %s {\n" def.struct_name;
        let iter (key, typ, attr) =
            let att = 
            ( match attr with 
            | Struct_Field -> "field"
            | Struct_Delegate -> "delegate"
            ) in
            Format.fprintf fmt ".%s : %a (%s)\n" key pp_ty typ att
        in 
        List.iter iter def.struct_fields;
        Format.fprintf fmt "}\n"

    | ADTDef(def) -> 
        Format.fprintf fmt "adt %s {\n" def.adt_name;
        let iter (key, typs) = 
            Format.fprintf fmt ".%s %a\n" key pp_tys typs
        in
        List.iter iter def.adt_branches;
        Format.fprintf fmt "}\n"
    | InterfaceDecl(decl) -> 
       Format.fprintf fmt "interface %s {\n" decl.intf_decl_name;

    | FunctionDef(impl) -> 
        pp_fun fmt impl

    | MethodsImpl(impl) -> 
        match (impl.impl_intf) with
        | Some(intf) -> 
            Format.fprintf fmt "impl %s for %s {\n" intf impl.impl_typ;
        | None -> 
            Format.fprintf fmt "impl %s {\n" impl.impl_typ;
        let iter func_impl = 
            pp_fun fmt func_impl
        in 
        List.iter iter impl.impl_methods;
        Format.fprintf fmt "}\n"
    ;;
