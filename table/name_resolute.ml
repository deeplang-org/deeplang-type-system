module NameMap = Map.Make(String);;
type name_map = symbol NameMap.t;;
(** deep & shallow -> discuss needed *)
type layered_name_map = 
    { deep    : name_map
    ; shallow : name_map
    }
    ;;
type var_data = 
    { name : string
    ; ty   : typ option
    (* ; id   : NodeId.expr *)
    }
    ;;
type var_table = (symbol, var_data) Hashtbl.t;;
type typ_data = 
    | Typ_Struct of struct_def
    | Typ_ADT of adt_def
    | Typ_Interface of interface_decl
    ;;
type typ_table = (typ_name, typ_data) Hashtbl.t;;

type table = 
    { var : var_table
    ; typ : typ_table
    (* ; ref : ref_table *)
    };;

(** TODO : ref_table *)
let rec walk_expr (expr:expr) (table:table) (name_map:name_map) : unit = match expr.shape with 
    | ExpVar(name)-> (match NameMap.find_opt name name_map with 
        | Some(_) -> () (* modify table.ref *)
        | None -> failwith("varaible Not Found") (* "^ name ^ " *)
    )
    | _ -> () (* recursively find name *)
    ;;

(** change table and name_map.deep, .shallow, .type *)
let walk_vpat (vpat:var_pattern) (table:table) (name_map:layered_name_map) : layered_name_map =
    let name = vpat.vpat_name in (
    match NameMap.find_opt name name_map.shallow with
        | Some(_) -> failwith("varaible"^ name ^ "has been declared")
        | None -> let key = vpat.vpat_symb in (
            Hashtbl.add table.var key
            { name = name
            ; ty   = None
            };
            (* return new name_map.deep, .shallow *)
            { deep = NameMap.add name key name_map.deep 
            ; shallow = NameMap.add name key name_map.shallow
            }
        )
    )
    ;;

let rec walk_pattern (pattern:pattern) (table:table) (name_map:layered_name_map) : layered_name_map = match pattern.shape with
    | PatWildcard -> name_map
    | PatVar(vpat) -> walk_vpat vpat table name_map
    | PatAs(pattern, vpat) -> 
        (* Typing rules here *)
        let name_map = walk_pattern pattern table name_map in
        walk_vpat vpat table name_map
    
    | PatADT(adt_label, patterns) -> 
        (* Typing rules here *)
        let walk_fold_left name_map pattern = walk_pattern pattern table name_map in
        List.fold_left walk_fold_left name_map patterns
    | PatStruct(typ_name, field_patterns)->
        (* Typing rules here *)
        let walk_fold_left name_map (struct_field, pattern) = walk_pattern pattern table name_map in
        List.fold_left walk_fold_left name_map field_patterns
    | PatTuple(patterns) ->
        (* Typing rules here *)
        let walk_fold_left name_map pattern = walk_pattern pattern table name_map in
        List.fold_left walk_fold_left name_map patterns
    ;;

let rec walk_stmt (stmt:stmt) (table:table) (name_map:layered_name_map) : layered_name_map = match stmt.shape with
    | StmtSeq(stmts) -> let walk_fold_left name_map stmt = walk_stmt stmt table name_map in
        List.fold_left walk_fold_left name_map stmts 
    | StmtExpr(expr) -> 
        walk_expr expr table name_map.deep;
        name_map
    | StmtDecl(pattern, expr) -> 
        let name_map = walk_pattern pattern table name_map in (
            walk_expr expr table name_map.deep;
            name_map
        )
    | StmtIf(cond, t_stmt, f_stmt) ->
        walk_expr cond table name_map.deep;
        walk_stmt t_stmt table name_map;
        walk_stmt f_stmt table name_map;
        name_map (* use the old *)
    | StmtFor((init, cond, final), body) ->
        let name_map = walk_stmt init table name_map in 
        ( walk_expr cond table name_map.deep
        ; walk_stmt body table name_map
        ; walk_stmt final table name_map
        );
        name_map (* use the old *)
    | StmtForRange((pattern, expr), body)->
        let name_map = walk_pattern pattern table name_map in (
            walk_expr expr table name_map.deep;
            walk_stmt body table name_map
        );
        name_map (* use the old *)
    | StmtWhile(cond, body) -> 
        walk_expr cond table name_map.deep;
        walk_stmt body table name_map;
        name_map (* use the old *)
    | StmtMatch(expr, branches) ->
        (* Typing rules here *)
        let walk_iter (pattern,body) = (
            let name_map = walk_pattern pattern table name_map in
            walk_stmt body;
            ()
        ) in List.iter walk_iter branches;
        name_map (* use the old *)
    | StmtReturn(expr) ->
        (* Typing rules here *)
        (* process control here *)
        name_map
    | StmtBreak ->
        (* process control here *)
        name_map
    | StmtContinue -> 
        (* process control here *)
        name_map
    ;;
(* TODO : func name check *)
let walk_func_decl (decl:func_decl) (table:table) (name_map:name_map) : name_map =
    let name = decl.func_decl_name in
    match NameMap.find_opt name name_map with
    | Some(_) -> failwith("function name " ^ name ^ " has been used")
    ;;

(** not return name_map *)
let walk_func_impl ((decl, stmt):func_impl) (table:table) (name_map:name_map) : unit = 
    (* Typing rules for func decl *)
    walk_func_decl decl table name_map;
    walk_stmt stmt table 
    { deep    = name_map
    ; shallow = NameMap.empty
    };
    ()
    ;;

(* impl method for T *)
let walk_method_decl = ();;
let walk_method_impl ((decl, stmt):func_impl) (table:table) (name_map:name_map) : unit = 
    (* Typing rules for func decl *)
    walk_func_decl decl table name_map;
    walk_stmt stmt table 
    { deep    = name_map
    ; shallow = NameMap.empty
    };
    ()
    ;;

let symbol_of_type = Symbol 0;;
let walk_top (clause:top_clause) (table:table) (name_map:name_map) = match clause.shape with 
    | GlobalVarDef(gvar) -> let key = gvar.gvar_id in 
        let name = gvar.gvar_name in (
            Hashtbl.add table.var key
            { name = name
            ; ty   = None
            };
            match NameMap.find_opt name name_map with
            | Some(_) -> failwith("The same global variable name")
            | None -> NameMap.add name key name_map
        )
    | StructDef(def) -> let key = def.struct_name in (
        Hashtbl.add table.typ key (Typ_Struct def);
        let name = def.struct_name in match NameMap.find_opt name name_map with
            | Some(_) -> failwith("The same Struct Name")
            | None -> NameMap.add name symbol_of_type name_map
    )
    | ADTDef(def) -> let key = def.adt_name in (
        Hashtbl.add table.typ key (Typ_ADT def);
        let name = def.adt_name in match NameMap.find_opt name name_map with
            | Some(_) -> failwith("The same ADT Name")
            | None -> NameMap.add name symbol_of_type name_map
    )
    | InterfaceDecl(decl) -> let key = decl.intf_decl_name in (
        Hashtbl.add table.typ key (Typ_Interface decl);
        let name = decl.intf_decl_name in match NameMap.find_opt name name_map with
            | Some(_) -> failwith("The same Interface Name")
            | None -> NameMap.add name symbol_of_type name_map
    )
    | FunctionDef(func_impl) -> 
        walk_func_impl func_impl table name_map;
        name_map
    | MethodsImpl(impl) ->
        (* Typing rules for MethodsImpl here*)
        let walk_iter func_impl = walk_func_impl func_impl table name_map in
        let methods = impl.impl_methods in
        List.iter walk_iter methods;
        name_map
    ;;



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
    ; typ=Hashtbl.create 10
    };;
walk_top test_case table NameMap.empty;;
table;;
