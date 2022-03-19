module NameMap = Map.Make(String);;
type name_map = symbol NameMap.t;;
(** deep & shallow -> discuss needed *)
type layered_name_map = 
    { mutable deep    : name_map
    ; mutable shallow : name_map
    }
    ;;
type var_data = 
    { mut  : mutability
    ; typ  : typ
    ; name : variable
    (* ; id   : NodeId.expr *)
    }
    ;;
type var_table = (symbol, var_data) Hashtbl.t;;

type fun_data =
    { args  : typ list
    ; retv  : typ
    ; name  : variable list (* args' names *)
    }
    ;;
type fun_table = (func_name, fun_data) Hashtbl.t;;

type adt_data = 
    { mutable intf : intf_name list (* impl interface for T *)
    ; mutable meth : fun_table      (* impl for T *)
    ;         core : (adt_label * typ list) list
    };;
type struct_data = 
    { mutable intf : intf_name list
    ; mutable meth : fun_table
    ;         core : struct_def_field list
    };;
type intf_data = 
    {         meth : fun_table
    };;
type typ_data =
    | ADT_data of adt_data
    | Struct_data of struct_data
    | Intf_data of intf_data
    ;;
type typ_table = (typ_name, typ_data) Hashtbl.t;; (* T -> impl what intf and what method *)
type adt_data = 
    { sum : typ_name (* sum_type's name *)
    ; typ : typ list
    }
    ;;
type adt_table = (adt_label, adt_data) Hashtbl.t;;

type ref_data = 
    { sym : symbol
    }
    ;;
type ref_table = (NodeId.expr, ref_data ) Hashtbl.t ;;
type table = 
    { var : var_table
    ; fnc : fun_table
    ; typ : typ_table
    ; adt : adt_table
    ; ref : ref_table
    };;

module BuiltinType = struct
    let bool : typ = 
        { shape = TyBool
        ; span  = None
        };;
    let i32 : typ = 
        { shape = TyInt(Signed,ISize_32)
        ; span  = None
        };;
    let f32 : typ =
        { shape = TyFloat(FSize_32)
        ; span  = None }
    let char : typ =
        { shape = TyChar
        ; span  = None
        };;
    let array (typ:typ) (size:int) : typ =
        { shape = TyArray(typ, size)
        ; span  = None
        };;
    let tuple (typs:typ list) : typ =
        { shape = TyTuple(typs)
        ; span  = None
        };;
    let var (name:typ_name) : typ =
        { shape = TyVar(name)
        ; span  = None (* Discuss : maybe not*)
        };;
    (** BuiltinType.this is just a hole *)
    let this : typ =
        { shape = TyThis
        ; span  = None
        };;
end
(* add variable in table and name_map.deep, .shallow *)
let walk_add_variable (symbol:symbol) (mut:mutability) (name:variable) (typo:typ option) (table:table) (name_map:layered_name_map) : unit = 
    match typo with
    | None -> failwith(" Type Annotation Needed Currently ")
    | Some(typ) -> (
        match NameMap.find_opt name name_map.shallow with
        | Some(_) -> failwith("varaible"^ name ^ "has been declared")
        | None -> Hashtbl.add table.var symbol
            { mut  = mut
            ; typ  = typ
            ; name = name
            }
        ;
        name_map.deep <- NameMap.add name symbol name_map.deep ;
        name_map.shallow <- NameMap.add name symbol name_map.shallow
    );;

(* walk pattern to modify table & name_map *)
(* TODO : pattern TyCk *)
let rec walk_pattern (pattern:pattern) (table:table) (name_map:layered_name_map) : unit = match pattern.shape with
    | PatWildcard -> ()
    | PatVar(vpat) -> walk_add_variable vpat.vpat_symb vpat.vpat_mut vpat.vpat_name vpat.vpat_typ table name_map
    | PatAs(pattern, vpat) -> 
        (* Typing rules here *)
        walk_pattern pattern table name_map;
        walk_add_variable vpat.vpat_symb vpat.vpat_mut vpat.vpat_name vpat.vpat_typ table name_map
    
    | PatADT(adt_label, patterns) -> 
        (* Typing rules here *)
        let walk_iter pattern = walk_pattern pattern table name_map in
        List.iter walk_iter patterns
    | PatStruct(itf_name, field_patterns)->
        (* Typing rules here *)
        let walk_iter (struct_field, pattern) = walk_pattern pattern table name_map in
        List.iter walk_iter field_patterns
    | PatTuple(patterns) ->
        (* Typing rules here *)
        let walk_iter pattern = walk_pattern pattern table name_map in
        List.iter walk_iter patterns
    ;;

let rec walk_expr (expr:expr) (table:table) (name_map:name_map) (type_this:typ): typ = match expr.shape with 
    | ExpLit(literal) -> ( match literal with
        | LitBool(_)   -> BuiltinType.bool
        | LitInt(_)    -> BuiltinType.i32  (* Discuss *)
        | LitFloat(_)  -> BuiltinType.f32  (* Discuss *)
        | LitChar(_)   -> BuiltinType.char 
        | LitString(s) -> BuiltinType.array BuiltinType.char (String.length s + 1) (* Discuss *)
    )
    | ExpVar(name)-> ( match NameMap.find_opt name name_map with 
        | Some(symbol) -> ( Hashtbl.add table.ref expr.expr_id
            { sym = symbol 
            };
            match Hashtbl.find_opt table.var symbol with
            | None -> failwith("Impossible, symbol not found, DEBUG needed")
            | Some(data) -> data.typ
        )
        | None -> failwith("varaible " ^ name ^ " Not Found")
    )
    | ExpUnOp(op, expr) -> let typ = walk_expr expr table name_map type_this in 
        ( match op with 
        | UnOpNeg -> ( match typ.shape with
            | TyInt(_, _) -> typ
            | TyFloat(_)  -> typ
            | TyChar      -> typ
            | _           -> failwith(" apply - to Not a Number ")
        )
        | UnOpNot -> ( match typ.shape with
            | TyBool -> typ
            | _      -> failwith(" apply - to Not a Boolean ")
        ))
    | ExpBinOp(op, left_e, right) ->
        let left  = walk_expr left_e table name_map type_this in
        let right = walk_expr right  table name_map type_this in
        let calculate_op_check (op:calculate_op) : typ = ( match op with
            | BinOpLOr | BinOpLAnd | BinOpLXor -> 
                if left=right then ( match right.shape with
                    | TyBool      -> BuiltinType.bool
                    | _           -> failwith(" logical operate non Bool ")
                )
                else failwith(" logical operate non Bool ")
            | BinOpBOr | BinOpBAnd | BinOpBXor ->
                if left=right then ( match right.shape with 
                    | TyInt(_, _) -> right (* Discuss *)
                    | _           -> failwith(" bits operate non Int ")
                )
                else failwith(" bits operate non Int ")
            | BinOpLShift | BinOpRShift -> ( match right.shape with 
                | TyInt(_, _) -> ( match left.shape with  (* Discuss *)
                    | TyInt(_, _) -> right (* Discuss *)
                    | _           -> failwith(" shifting non Int ")
                )
                | _           -> failwith(" shifting a NaN bits ")
            )
            | BinOpAdd | BinOpSub | BinOpMul | BinOpDiv ->
                if left=right then ( match right.shape with 
                    | TyInt(_, _) | TyFloat(_)  
                        -> right
                    | _ -> failwith(" arithmetic on neither Int nor Float ")
                )
                else failwith(" arithmetic on different types")
            | BinOpMod -> 
                if left=right then ( match right.shape with 
                    | TyInt(_, _) -> right
                    | _           -> failwith(" mod on non Int ")
                )
                else failwith(" mod on different types")
            ) in
        ( match op with
        | BinOpCompare(op) -> ( match op with
            | BinOpLt | BinOpLeq | BinOpGt | BinOpGeq -> 
                if left=right then ( match right.shape with
                    | TyInt(_, _) | TyFloat(_) | TyChar      
                        -> BuiltinType.bool
                    | _ -> failwith(" ordering values from none of Int, Float, Char ")
                )
                else failwith(" ordering values from different types")
            | BinOpEq | BinOpNeq -> 
                if left=right then 
                    BuiltinType.bool
                else failwith(" equaling values from different types")
        )
        | BinOpCalculate(op) -> calculate_op_check op
        | BinOpAssign(opo) -> ( match left_e.shape with
            | ExpVar(_) -> ()
            | _         -> failwith(" assign a value to a non variable name")
            );
            ( match opo with
            | None -> if left=right then right 
                else failwith(" assign a value:T1 to a variable:T2, while T1!=T2")
            | Some(op) -> calculate_op_check op
            )
        )
    | ExpTuple(exprs) -> BuiltinType.tuple ( List.map (fun expr -> walk_expr expr table name_map type_this) exprs )
    | ExpAdt(label, exprs) -> ( match Hashtbl.find_opt table.adt label with
        | None       -> failwith(" ADT label " ^ label ^ " Not Found ")
        | Some(data) -> (
            if data.typ = List.map (fun expr -> walk_expr expr table name_map type_this) exprs then
                BuiltinType.var data.sum
            else
                failwith(" types doesn't match with ADT label "^ label)
        ))
    | ExpStruct(name, field_exprs) -> ( match Hashtbl.find_opt table.typ name with
        | None -> failwith(" type " ^ name ^ " Not Found ")
        | Some(ADT_data(_)) -> failwith(" type " ^ name ^ " is not a Strcut but an ADT ")
        | Some(Intf_data(_)) -> failwith(""^name ^ " is not a Strcut but an Interface ")
        | Some(Struct_data(data)) -> ( let def_fields = data.core in
                let map_def_field (def_field:struct_def_field) = match def_field with
                    | StructField(field, typ) -> (field, typ)
                    | StructDelegate(field, typ) -> (field, typ)
                in 
                let map_field_expr ((field, expr)) = 
                    (field, walk_expr expr table name_map type_this)
                in
                if List.map map_def_field def_fields = List.map map_field_expr field_exprs then
                    BuiltinType.var name
                else
                    failwith(" types doesn't match with fields of Struct " ^ name)
        ))
    | ExpField(expr, field) -> let rec find_field (typ:typ) (field:struct_field) : typ option =
        ( match typ.shape with 
        | TyVar(name) -> 
            ( match Hashtbl.find_opt table.typ name with
            | Some(Struct_data(data)) -> 
                let def_fields = data.core in
                let find_map def_field = ( match def_field with
                    | StructField(field', typ)
                    | StructDelegate(field', typ) 
                    -> if field'=field then Some typ else None
                ) in 
                (* find in this level first *)
                ( match List.find_map find_map def_fields with 
                | Some(typ) -> Some(typ)
                (* if failed, try to find it in delegated fields *)
                | None ->  let types = List.filter_map
                    (fun field -> match field with
                        | StructField(_, _) -> None
                        | StructDelegate(_, typ) -> Some(typ)
                    ) def_fields in 
                    let find_map' (typ:typ) = find_field typ field in
                        List.find_map find_map' types
                )
            | _ -> None
            )
        | _           -> None (* failwith(" expr is not a term of struct type") *)
        ) in 
        let typ = walk_expr expr table name_map type_this in
        ( match typ.shape with 
        | TyVar(name) -> ( match Hashtbl.find_opt table.typ name with
            | None       -> failwith("It's impossible. DEBUG why struct name not found")
            | Some(ADT_data(data)) -> failwith(" ADT has no fields! ")
            | Some(Intf_data(_)) -> failwith(" Interface has no fields! ")
            | Some(Struct_data(_)) -> ( match find_field typ field with
                | None -> failwith(" field " ^ field ^ " not found ")
                | Some(typ) -> typ
                )
            )
        | _        -> failwith(" expr is not a term of struct type")
        )
    | ExpThis -> type_this;
    | ExpApp(func, args) -> (* interface check included *)
        ( match Hashtbl.find_opt table.fnc func with
        | None -> failwith(" function " ^ func ^ " not found ")
        | Some(data) -> 
            (* arg : formal, i.e. declared in function , para : actual *)
            let ty_eq_with_intf (para_t:typ) (arg_t:typ) = 
                if para_t = arg_t then true 
                else match arg_t.shape with (* para_t = Type <: Intf = arg_t *)
                | TyVar(intf_name) -> (match Hashtbl.find_opt table.typ intf_name with
                    | None -> failwith("Impossible, DEBUG please")
                    (* arg_t is a interface *)
                    | Some(Intf_data(_)) -> ( match para_t.shape with
                        | TyVar(type_name) -> ( match Hashtbl.find_opt table.typ type_name with
                            | None -> failwith("Impossible, DEBUG please")
                            | Some(Intf_data(_)) -> false (* REMAIN : interface subtyping here, currently reject *)
                            | Some(Struct_data(data)) -> List.exists (fun name->name=intf_name) data.intf;
                            | Some(ADT_data(data)) -> List.exists (fun name->name=intf_name) data.intf;
                        )
                        | _ -> false
                    )
                    | _ -> false
                )
                | _ -> false
            in
            let para_types = List.map (fun expr->walk_expr expr table name_map type_this) args in
            let result = List.for_all2 ty_eq_with_intf para_types data.args in
            if result then
                data.retv
            else failwith(" function args' types doesn't match ")
    )
    | ExpMethod(obj, meth, args) -> (
        let obj_type = walk_expr obj table name_map type_this in
        match obj_type.shape with
        | TyVar(name) -> ( let walk_meth fun_table = match Hashtbl.find_opt fun_table meth with
            | None -> failwith(" method " ^ meth ^ " not found in type " ^ name)
            | Some(data) -> 
                if data.args = List.map (fun expr->walk_expr expr table name_map type_this) args then
                    data.retv
                else failwith(" function args' types doesn't match ")
            in 
            match Hashtbl.find_opt table.typ name with
            | None -> failwith(" method not found for type " ^ name)
            | Some(ADT_data(data)) -> walk_meth data.meth
            | Some(Struct_data(data)) -> walk_meth data.meth
            | Some(Intf_data(data)) -> walk_meth data.meth
        )
        | _ -> failwith(" Unsupported ") (* Discuss : Support impl for just TyVar currently *)
    )
    | ExpIf(cond, fst, snd) -> let cond = walk_expr cond table name_map type_this in
        ( match cond.shape with
        | TyBool -> ()
        | _      -> failwith(" condition of if is not a bool ")
        )
        ;
        let fst = walk_expr fst table name_map type_this in
        let snd = walk_expr snd table name_map type_this in
        if fst=snd then snd else failwith(" types of fst and snd of if don't equal ")
    | ExpMatch(expr, pattern_exprs) -> let typ = walk_expr expr table name_map type_this in
        (* Discuss : Only support match term:ADT with ...
         * as for other pattern matches, use let pattern = term:Type
         *)
        ( match typ.shape with
        | TyVar(name) -> ( match Hashtbl.find_opt table.typ name with
            | None       -> failwith("It's impossible. DEBUG why ADT name not found")
            | Some(Intf_data(_)) -> failwith("cannot match with Interface")
            | Some(Struct_data(_)) -> failwith(" cannot match with Struct")
            | Some(ADT_data(data)) -> let branches = data.core in
                (* TODO Discuss : how to solve pattern exhaustive not only here but also StmtMatch *)
                (* Discussion : Temporarily not implement. *)
                (* branches unused because pattern exhaustion hasn't been done *)
                (* check all exprs from pattern_exprs : the same type
                 * then return it
                 *)
                let mapper (pattern, expr) =  
                    let layered_name_map : layered_name_map = 
                    { deep = name_map
                    ; shallow = NameMap.empty
                    } in (
                        walk_pattern pattern table layered_name_map;
                        walk_expr expr table layered_name_map.deep BuiltinType.this
                ) in
                let types = List.map mapper pattern_exprs in
                let ty1 = match types with
                    | [] -> failwith(" match no branches ")
                    | t::tys -> t 
                in 
                if List.for_all (fun ty'->ty'=ty1) types then
                    ty1
                else
                    failwith(" match branches return different types");
            ) 
        | _           -> failwith(" expr is not a term of ADT")
        (* As for tuple, use let assignment to fetch its sub-fields *)
        )
    ;;

(** only used by walk_stmt in return type checking *)
let eq_typo (ty1:typ option) (ty2:typ option) = match ty1 with
    | None -> true
    | Some(ty1) -> (match ty2 with 
        | None -> true
        | Some(ty2) -> ty1=ty2
    );;
(* walk_stmt : ... -> typ list *)
let rec walk_stmt (stmt:stmt) (table:table) (name_map:layered_name_map) (typo:typ option) (type_this:typ): typ option = match stmt.shape with
    | StmtSeq(stmts) -> 
        let walk_fold_left typo stmt = walk_stmt stmt table name_map typo type_this in
        let typo' = List.fold_left walk_fold_left typo stmts in
        typo'
    | StmtExpr(expr) -> 
        let ty = walk_expr expr table name_map.deep type_this in
        typo
    | StmtDecl(pattern, expr) -> 
        let ty = walk_expr expr table name_map.deep type_this in 
        walk_pattern pattern table name_map;
        typo
    | StmtIf(cond, t_stmt, f_stmt) ->
        let ty = walk_expr cond table name_map.deep type_this in
        (* WARNING, use '=' here ... *)
        if ty = BuiltinType.bool then ()
        else failwith(" cond of if-statement is not a boolean"); 
        let typo' = walk_stmt t_stmt table name_map typo type_this in
        let typo' = walk_stmt f_stmt table name_map typo' type_this in
        typo'
    | StmtFor((init, cond, final), body) ->
        let ty = walk_expr cond table name_map.deep type_this in
        if ty = BuiltinType.bool then 
            let typo' = walk_stmt init table name_map typo type_this in
            let typo' = walk_stmt body table name_map typo' type_this in
            let typo' = walk_stmt final table name_map typo' type_this in
            typo'
        else failwith(" cond of if-statement is not a boolean"); 
    | StmtForRange((pattern, expr), body)->
        let ty = walk_expr expr table name_map.deep type_this in
        let _ = walk_pattern pattern table name_map in
        let typo' = walk_stmt body table name_map typo type_this in
        typo'
    | StmtWhile(cond, body) -> 
        let ty = walk_expr cond table name_map.deep type_this in
        if ty = BuiltinType.bool then 
            let typo' = walk_stmt body table name_map typo type_this in
                typo'
        else failwith(" cond of if-statement is not a boolean"); 
    | StmtMatch(expr, branches) ->
        let ty = walk_expr expr table name_map.deep type_this in
        let walk_fold_left typo (pattern,body) = (
            walk_pattern pattern table name_map;
            walk_stmt body table name_map typo type_this
        ) in List.fold_left walk_fold_left typo branches
    | StmtReturn(expr) ->
        (* process control here *)
        let typo' = Some (walk_expr expr table name_map.deep type_this) in
        if eq_typo typo typo' then typo'
        else failwith(" return different types ")
    | StmtBreak ->
        (* process control here *)
        typo
    | StmtContinue -> 
        (* process control here *)
        typo
    ;;

(** not return name_map *)

(* check func name, and collect type info *)
let walk_func_decl (name:func_name) (args:func_arg list) (retv:typ) (table:table) : unit =
    match Hashtbl.find_opt table.fnc name with
    | Some(_) -> failwith("function name " ^ name ^ " has been used")
    | None -> ( Hashtbl.add table.fnc name
        { args=List.map (fun (arg:func_arg)->arg.farg_typ) args
        ; retv=retv
        ; name=List.map (fun (arg:func_arg)->arg.farg_name) args
        }
    )
    ;;
let walk_func_stmt (type_this:typ) (args:func_arg list) (stmt:stmt) (table:table) (name_map:name_map) =
    let layered_map =     
        { deep    = name_map
        ; shallow = NameMap.empty
        } in
    let walk_iter (arg:func_arg) = 
        walk_add_variable arg.farg_symb Mut arg.farg_name (Some arg.farg_typ) table layered_map
        in
    let _ = List.iter walk_iter args in
    walk_stmt stmt table layered_map None type_this;;

(* impl intf for T { methods }*)
(* check intf name *)
let walk_method_intf (intf:intf_name option) (typ:typ_name) (table:table) : unit = 
    match intf with
    | None -> () 
    (* no interface name, no need to check whether impl methods 
     * declared in interface
     *)
    | Some(name) -> ( match Hashtbl.find_opt table.typ name with
        | Some(Intf_data data) -> 
            let folder fun_name fun_data bool = 
                if bool then match Hashtbl.find_opt table.typ typ with
                    | Some(ADT_data(data)) -> ( match Hashtbl.find_opt data.meth fun_name with
                        | None -> false
                        | Some(fun_data') -> 
                            fun_data.args = fun_data'.args 
                         && fun_data.retv = fun_data'.retv
                        )
                    | Some(Struct_data(data)) -> ( match Hashtbl.find_opt data.meth fun_name with
                        | None -> false
                        | Some(fun_data') -> 
                            fun_data.args = fun_data'.args 
                         && fun_data.retv = fun_data'.retv
                        )
                    | _ -> failwith(" impl type NOT FOUND ")
                else false in
            let result = Hashtbl.fold folder data.meth true in 
            if result then ()
            else failwith(" not match with interface's methods ")
        | Some(ADT_data(_)) -> failwith(name ^ " is not an interface but an ADT")
        | Some(Struct_data(_)) -> failwith(name ^ " is not an interface but a Struct")
        | None -> failwith("no interface name as " ^ name)
    )
    ;;

(* check method name, and collect type info *)
let walk_method_decl (typ:typ_name) (name:func_name) (args:func_arg list) (retv:typ) (table:table) : unit =
    let fun_table = match Hashtbl.find_opt table.typ typ with
        | None -> failwith("no type named" ^ typ)
        | Some(Intf_data(table)) -> failwith(typ^" is not a type but an interface")
        | Some(Struct_data(table)) -> table.meth
        | Some(ADT_data(table)) -> table.meth
    in
    match Hashtbl.find_opt fun_table name with
    | Some(_) -> failwith("member function name " ^ name ^ " has been used")
    | None -> ( Hashtbl.add fun_table name
        { args=List.map (fun (arg:func_arg)->arg.farg_typ) args
        ; retv=retv
        ; name=List.map (fun (arg:func_arg)->arg.farg_name) args
        }
    )
    ;;
let walk_method_stmt = walk_func_stmt;;

let walk_top (clause:top_clause) (table:table) (name_map:name_map) = match clause.shape with 
    | GlobalVarDef(gvar) -> let symbol = gvar.gvar_id in 
        let name = gvar.gvar_name in (
            Hashtbl.add table.var symbol
            { mut  = Mut
            ; typ  = walk_expr gvar.gvar_value table name_map BuiltinType.this
            ; name = name
            };
            match NameMap.find_opt name name_map with
            | Some(_) -> failwith("The same global variable name")
            | None -> NameMap.add name symbol name_map
        )
    | StructDef(def) -> (
        let name = def.struct_name in 
        let fields = def.struct_fields in
        match Hashtbl.find_opt table.typ name with
        | Some(_) -> failwith("The same Struct Name")
        | None -> Hashtbl.add table.typ name (Struct_data(
            { intf = []
            ; meth = Hashtbl.create 10
            ; core = fields
            }
        ))
        ;
        name_map
    )
    | ADTDef(def) -> 
        let name = def.adt_name in 
        let branches = def.adt_branches in
        ( match Hashtbl.find_opt table.typ name with
        | Some(_) -> failwith("The same ADT Name")
        | None    -> ( Hashtbl.add table.typ name (ADT_data (
            { intf = []
            ; meth = Hashtbl.create 10
            ; core = branches
            }));
            let walk_iter_adt_branch ((label, typs)) = 
                ( match Hashtbl.find_opt table.adt label with
                | Some(_) -> failwith("The same ADT label")
                | None    -> Hashtbl.add table.adt label
                    { sum = name
                    ; typ = typs
                    }
                ) in
            List.iter walk_iter_adt_branch branches
        ));
        name_map
    | InterfaceDecl(decl) -> let name = decl.intf_decl_name in
        ( match Hashtbl.find_opt table.typ name with
        | Some(_) -> failwith("The same Interface Name")
        | None ->
            let fun_table = Hashtbl.create 10 in
            let iterator (decl:func_decl) = 
                let args = List.map (fun farg->farg.farg_typ) decl.func_decl_args in
                let name = List.map (fun farg->farg.farg_name) decl.func_decl_args in
                Hashtbl.add fun_table decl.func_decl_name
                { args = args
                ; retv = decl.func_decl_ret
                ; name = name
                } in 
            ( List.iter iterator decl.intf_decl_methods
            ; Hashtbl.add table.typ name (Intf_data { meth = fun_table })
            )
        );
        name_map
    | FunctionDef(func_impl) ->
        let (decl, stmt) = func_impl in
        let args = decl.func_decl_args in
        let name = decl.func_decl_name in
        let retv = decl.func_decl_ret  in (
            walk_func_decl name args retv table; (* func name check, modify table.fnc *)
            let typo = walk_func_stmt BuiltinType.this args stmt table name_map in
            if typo = (Some retv) then ()
            else failwith(" function return type doesn't match ! ")
        );
        name_map
    | MethodsImpl(impl) -> 
        (* Typing rules for MethodsImpl here*)
        let typ = impl.impl_typ in
        let methods = impl.impl_methods in
        let intf = impl.impl_intf in (

            let walk_iter ((decl, stmt):func_impl) = (
                let args = decl.func_decl_args in
                let name = decl.func_decl_name in
                let retv = decl.func_decl_ret  in (
                    walk_method_decl typ name args retv table;
                    let typo = walk_method_stmt (BuiltinType.var typ) args stmt table name_map in
                    if typo = (Some retv) then ()
                    else failwith(" function return type doesn't match ! ")
                )
            ) in List.iter walk_iter methods;
            walk_method_intf intf typ table;
        );
        name_map
    ;;



(* test *)
let mk_id = let counter = ref 0 in
    fun () -> incr counter; !counter
    ;;
let test_span : src_span = 
    { file="test"
    ; row_s=0
    ; col_s=0
    ; row_e=0
    ; col_e=0
    };;
let top_clause_gen shape : top_clause = 
    { shape=shape
    ; span=test_span
    };;
let expr_gen shape : expr =
    { shape=shape
    ; expr_id= NodeId.ExprId (mk_id ())
    ; span=test_span
    };;
let test_case = top_clause_gen (GlobalVarDef
    { gvar_name="x"
    ; gvar_id=Symbol (mk_id ())
    ; gvar_value=expr_gen (ExpLit (LitInt 3))
    });;

let table : table = 
    { var=Hashtbl.create 10
    ; fnc=Hashtbl.create 10
    ; typ=Hashtbl.create 10
    ; adt=Hashtbl.create 10
    ; ref=Hashtbl.create 10
    };;
walk_top test_case table NameMap.empty;;
table;;
