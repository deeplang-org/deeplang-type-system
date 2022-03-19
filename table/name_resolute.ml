module NameMap = Map.Make(String);;
type name_map = symbol NameMap.t;;
(** deep & shallow -> discuss needed *)
type layered_name_map = 
    { deep    : name_map
    ; shallow : name_map
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
    }
    ;;
type fun_table = (func_name, fun_data) Hashtbl.t;;

type itf_data = func_decl list;;
type itf_table = (intf_name, itf_data) Hashtbl.t;;

type typ_core = 
    | StructCore of struct_def_field       list
    | ADTCore    of (adt_label * typ list) list
    ;;
type typ_data = 
    { mutable intf : intf_name list (* impl interface for T *)
    ; mutable meth : fun_table      (* impl for T *)
    ;         core : typ_core       
    }
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
    ; itf : itf_table
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
        ; span  = None (* TODO : save it when in top_clause *)
        };;
    (** BuiltinType.this is just a hole *)
    let this : typ =
        { shape = TyThis
        ; span  = None
        };;
end
(* add variable in table and name_map.deep, .shallow *)
let walk_add_variable (symbol:symbol) (mut:mutability) (name:variable) (typo:typ option) (table:table) (name_map:layered_name_map) : layered_name_map = 
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
        { deep    = NameMap.add name symbol name_map.deep 
        ; shallow = NameMap.add name symbol name_map.shallow
        }
    );;

(* walk pattern to modify table & name_map *)
let rec walk_pattern (pattern:pattern) (table:table) (name_map:layered_name_map) : layered_name_map = match pattern.shape with
    | PatWildcard -> name_map
    | PatVar(vpat) -> walk_add_variable vpat.vpat_symb vpat.vpat_mut vpat.vpat_name vpat.vpat_typ table name_map
    | PatAs(pattern, vpat) -> 
        (* Typing rules here *)
        let name_map = walk_pattern pattern table name_map in
        walk_add_variable vpat.vpat_symb vpat.vpat_mut vpat.vpat_name vpat.vpat_typ table name_map
    
    | PatADT(adt_label, patterns) -> 
        (* Typing rules here *)
        let walk_fold_left name_map pattern = walk_pattern pattern table name_map in
        List.fold_left walk_fold_left name_map patterns
    | PatStruct(itf_name, field_patterns)->
        (* Typing rules here *)
        let walk_fold_left name_map (struct_field, pattern) = walk_pattern pattern table name_map in
        List.fold_left walk_fold_left name_map field_patterns
    | PatTuple(patterns) ->
        (* Typing rules here *)
        let walk_fold_left name_map pattern = walk_pattern pattern table name_map in
        List.fold_left walk_fold_left name_map patterns
    ;;

(** TODO : modify ref_table *)
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
        | Some(data) -> ( match data.core with
            | ADTCore(_) -> failwith(" type " ^ name ^ " is not a Strcut but an ADT ")
            | StructCore(def_fields) ->
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
            | None       -> None (* failwith("It's impossible. DEBUG why typ_name not found") *)
            | Some(data) -> ( match data.core with 
                | ADTCore(_) -> None (* failwith(" ADT has no fields! ") *)
                | StructCore(def_fields) ->
                    let find_map def_field = ( match def_field with
                        | StructField(field', typ)
                        | StructDelegate(field', typ) 
                        -> if field'=field then Some typ else None
                    ) in 
                    (* find in this level first *)
                    match List.find_map find_map def_fields with 
                    | Some(typ) -> Some(typ)
                    (* if failed, try to find it in delegated fields *)
                    | None -> ( let types = List.filter_map
                        (fun field -> match field with
                            | StructField(_, _) -> None
                            | StructDelegate(_, typ) -> Some(typ)
                        ) def_fields in 
                        let find_map' (typ:typ) = find_field typ field in
                            List.find_map find_map' types
                    )
                )
            )
        | _           -> None (* failwith(" expr is not a term of struct type") *)
        ) in 
        let typ = walk_expr expr table name_map type_this in
        ( match typ.shape with 
        | TyVar(name) -> ( match Hashtbl.find_opt table.typ name with
            | None       -> failwith("It's impossible. DEBUG why struct name not found")
            | Some(data) -> ( match data.core with 
                | ADTCore(_) -> failwith(" ADT has no fields! ")
                | StructCore(_) -> ( match find_field typ field with
                    | None -> failwith(" field " ^ field ^ " not found ")
                    | Some(typ) -> typ
                    )
                )
            ) 
        | _        -> failwith(" expr is not a term of struct type")
        )
    | ExpThis -> type_this;
    | ExpApp(func, args) -> (* TODO : interface check haven't included *)
        ( match Hashtbl.find_opt table.fnc func with
        | None -> failwith(" function " ^ func ^ " not found ")
        | Some(data) -> 
            if data.args = List.map (fun expr->walk_expr expr table name_map type_this) args then
                data.retv
            else failwith(" function args' types doesn't match ")
    )
    | ExpMethod(obj, meth, args) -> (
        let obj_type = walk_expr obj table name_map type_this in
        match obj_type.shape with
        | TyVar(name) -> ( match Hashtbl.find_opt table.typ name with
            | None -> failwith(" method not found for type " ^ name);
            | Some(data) -> ( match Hashtbl.find_opt data.meth meth with 
                | None -> failwith(" method " ^ meth ^ " not found in type " ^ name);
                | Some(data) -> 
                    if data.args = List.map (fun expr->walk_expr expr table name_map type_this) args then
                        data.retv
                    else failwith(" function args' types doesn't match ")
            )
        )
        | _ -> failwith(" Unsupported ") (* TODO Discuss whether or not to support impl method for Other Types ?? *)
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
            | Some(data) -> ( match data.core with 
                | StructCore(_) -> failwith(" cannot match with Struct")
                | ADTCore(branches) -> (
                (* TODO Discuss : how to solve pattern exhaustive *)
                (* Discussion : Temporarily not implement. *)

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
            )
        ) 
        | _           -> failwith(" expr is not a term of ADT")
        (* As for tuple, use let assignment to fetch its sub-fields *)
        )
    ;;

let rec walk_stmt (stmt:stmt) (table:table) (name_map:layered_name_map) (type_this:typ): layered_name_map = match stmt.shape with
    | StmtSeq(stmts) -> let walk_fold_left name_map stmt = walk_stmt stmt table name_map type_this in
        List.fold_left walk_fold_left name_map stmts 
    | StmtExpr(expr) -> 
        walk_expr expr table name_map.deep type_this;
        name_map
    | StmtDecl(pattern, expr) -> 
        let name_map = walk_pattern pattern table name_map in (
            walk_expr expr table name_map.deep type_this;
            name_map
        )
    | StmtIf(cond, t_stmt, f_stmt) ->
        walk_expr cond table name_map.deep type_this;
        walk_stmt t_stmt table name_map type_this;
        walk_stmt f_stmt table name_map type_this;
        name_map (* use the old *)
    | StmtFor((init, cond, final), body) ->
        let name_map = walk_stmt init table name_map type_this in 
        ( walk_expr cond table name_map.deep type_this
        ; walk_stmt body table name_map type_this
        ; walk_stmt final table name_map type_this
        );
        name_map (* use the old *)
    | StmtForRange((pattern, expr), body)->
        let name_map = walk_pattern pattern table name_map in (
            walk_expr expr table name_map.deep type_this;
            walk_stmt body table name_map type_this
        );
        name_map (* use the old *)
    | StmtWhile(cond, body) -> 
        walk_expr cond table name_map.deep type_this;
        walk_stmt body table name_map type_this;
        name_map (* use the old *)
    | StmtMatch(expr, branches) ->
        (* Typing rules here *)
        let walk_iter (pattern,body) = (
            let name_map = walk_pattern pattern table name_map in
            walk_stmt body table name_map type_this;
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

(** not return name_map *)

(* check func name, and collect type info *)
let walk_func_decl (name:func_name) (args:func_arg list) (retv:typ) (table:table) : unit =
    match Hashtbl.find_opt table.fnc name with
    | Some(_) -> failwith("function name " ^ name ^ " has been used")
    | None -> ( Hashtbl.add table.fnc name
        { args=List.map (fun (arg:func_arg)->arg.farg_typ) args
        ; retv=retv
        }
    )
    ;;
let walk_func_stmt (type_this:typ) (args:func_arg list) (stmt:stmt) (table:table) (name_map:name_map) =
    let walk_fold_left (layered_map:layered_name_map) (arg:func_arg) = 
        walk_add_variable arg.farg_symb Mut arg.farg_name (Some arg.farg_typ) table layered_map
        in
    let layer_map = List.fold_left walk_fold_left
    { deep    = name_map
    ; shallow = NameMap.empty
    } args in
    walk_stmt stmt table layer_map type_this;;

(* impl intf for T { methods }*)
(* check intf name *)
let walk_method_intf (intf:intf_name option) (typ:typ_name) (methods:func_impl list) (table:table) : unit = 
    match intf with
    | None -> () 
    (* no interface name, no need to check whether impl methods 
     * declared in interface
     *)
    | Some(name) -> ( match Hashtbl.find_opt table.itf name with
        | Some(decls) -> 
            let method_decls = List.map (fun ((decl, _) : func_impl)-> decl) methods in
            let has_method (decl:func_decl) : bool = List.exists  (fun method_decl->
                method_decl.func_decl_name = decl.func_decl_name
             && method_decl.func_decl_args = decl.func_decl_args
             && method_decl.func_decl_ret  = decl.func_decl_ret
            ) method_decls in
            if List.for_all has_method decls = false then
                failwith("not impl all methods declared in interface " ^ name)
            else ( match Hashtbl.find_opt table.typ typ with
                | None -> failwith("no type named" ^ typ)
                | Some(typ_table) -> ( match List.find_opt (fun name'->name'=name) typ_table.intf  with
                    | Some(_) -> failwith("interface " ^ name ^ "has been implemented")
                    | None -> ( typ_table.intf = name::typ_table.intf; () )
                )
            )
        | _ -> failwith("no interface name as " ^ name)
    )
    ;;

(* check method name, and collect type info *)
let walk_method_decl (typ:typ_name) (name:func_name) (args:func_arg list) (retv:typ) (table:table) : unit =
    let fun_table = (match Hashtbl.find_opt table.typ typ with
        | None -> failwith("no type named" ^ typ)
        | Some(typ_table) -> typ_table.meth
    ) in
    match Hashtbl.find_opt fun_table name with
    | Some(_) -> failwith("member function name " ^ name ^ " has been used")
    | None -> ( Hashtbl.add fun_table name
        { args=List.map (fun (arg:func_arg)->arg.farg_typ) args
        ; retv=retv
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
        | None -> Hashtbl.add table.typ name 
            { intf = []
            ; meth = Hashtbl.create 10
            ; core = StructCore fields
            }
        ;
        name_map
    )
    | ADTDef(def) -> 
        let name = def.adt_name in 
        let branches = def.adt_branches in
        ( match Hashtbl.find_opt table.typ name with
        | Some(_) -> failwith("The same ADT Name")
        | None    -> ( Hashtbl.add table.typ name 
            { intf = []
            ; meth = Hashtbl.create 10
            ; core = ADTCore branches
            }
            ;
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
        ( match Hashtbl.find_opt table.itf name with
        | Some(_) -> failwith("The same Interface Name")
        | None -> Hashtbl.add table.itf name decl.intf_decl_methods
        );
        name_map
    | FunctionDef(func_impl) ->
        let (decl, stmt) = func_impl in
        let args = decl.func_decl_args in
        let name = decl.func_decl_name in
        let retv = decl.func_decl_ret  in (
            walk_func_decl name args retv table; (* func name check, modify table.fnc *)
            walk_func_stmt BuiltinType.this args stmt table name_map
        );
        name_map
    | MethodsImpl(impl) -> 
        (* Typing rules for MethodsImpl here*)
        let typ = impl.impl_typ in
        let methods = impl.impl_methods in
        let intf = impl.impl_intf in (
            walk_method_intf intf typ methods table;

            let walk_iter ((decl, stmt):func_impl) = (
                let args = decl.func_decl_args in
                let name = decl.func_decl_name in
                let retv = decl.func_decl_ret  in (
                    walk_method_decl typ name args retv table;
                    walk_method_stmt (BuiltinType.var typ) args stmt table name_map;
                    ()
                )
            ) in List.iter walk_iter methods;
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
    ; itf=Hashtbl.create 10
    ; typ=Hashtbl.create 10
    ; adt=Hashtbl.create 10
    ; ref=Hashtbl.create 10
    };;
walk_top test_case table NameMap.empty;;
table;;
