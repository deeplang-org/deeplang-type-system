open Syntax.ParseTree;;
open Table;;
open SemanticsError;;

(** This module walks AST from {! Syntax.ParseTree},
    to do {b name resolution} and {b type checking} in one pass.
  *)

(** {1 Context} *)

type nametbl = (variable, symbol) Hashtbl.t ;;
type scope = nametbl list ;;
(* type loop = inloop | notinloop ;; *)
type context = 
    {         table   : table
    ; mutable nametbl : nametbl 
    ; mutable scope   : scope    (** nametbl::scope *)
    ; mutable this    : typ      (** type This of one struct or ADT *)
    ; mutable rety    : typ      (** return type of function *)
    ; mutable checkloop  : int
    };;


(** {1 Helper} *)

(** Those used in {! Walker}, which is not yet defined in {! Syntax.ParseTree} *)

(** {1 Implementation} *)

(** scope block \{ stmt_list \} *)

let scope_beg (context:context) : unit = 
    context.scope <- context.nametbl::context.scope ;
    context.nametbl <- Hashtbl.create 10
    ;;
let scope_end (context:context) : unit = 
    match context.scope with
    | nametbl::scope -> 
        context.nametbl <- nametbl ;
        context.scope <- scope 
    | [] -> error_type (Error "Impossible scope empty")
    ;;


(** add variable into nametbl and table *)

let add_variable (context:context) (symbol:symbol) (mut:mutability) (name:variable) (typo:typ option) : unit = 
    let table = context.table in 
    let nametbl = context.nametbl in
    let typ = match typo with
        | None -> error_type (Error (" Type Annotation Needed Currently "))
        | Some(typ) -> typ
        in
    match Hashtbl.find_opt nametbl name with
        | Some(_) -> error_type (Error ("varaible "^ name ^ " has been declared"))
        | None -> Hashtbl.add table.var symbol
            { mut  = mut
            ; typ  = typ
            ; name = name
            }
        ;
        Hashtbl.add nametbl name symbol
    ;;
let find_var_opt (context:context) (name:variable) : symbol option = 
    match Hashtbl.find_opt context.nametbl name with
    | Some(symbol) -> Some(symbol)
    | None -> let rec find (scope:scope) = 
        ( match scope with 
        | nametbl::scope -> 
            ( match Hashtbl.find_opt nametbl name with
            | Some(symbol) -> Some(symbol)
            | None -> find scope
            )
        | [] -> None
        ) in 
        find context.scope
    ;;

(** call add_variable 
    {[
    # walk_pattern context pattern typ
    ]}
    [typ] is the type of expr corresponding to [pattern]

    In [ let (x, _) = (3, 1.1) ], [pattern] is [(x, _)], and [typ] is [(i64, f64)].
*)

let unsupport_generics (tys:typ list) = 
    ( match tys with 
    | [] -> () 
    | _::_ -> error_type (Error "Unsupport for Generics Programming")
    )
    ;;

(** check TyNamed whether in context.typ *)
let rec walk_type (context:context) (typ:typ) : bool =
    let table = context.table.typ in
    match typ.shape with 
    | TyNamed(name, tys) -> 
        unsupport_generics tys;
        ( match Hashtbl.find_opt table name with
        | None -> error_type (TypeError (typ, "Type Named "^name^" Not Found"))
        | Some(_) -> true
        )
    | TyVar(_) -> error_type (TypeError (typ, "Unsupport for Generics Programming"))
    | TyArray(typ, _) -> walk_type context typ
    | TyTuple(tys) -> List.for_all (walk_type context) tys
    | _ -> true
    ;;

let rec walk_pattern (context:context) (pattern:pattern) (typ:typ) : unit = 
    let table = context.table in 
    match pattern.shape with
    | PatWildcard -> ()
    | PatLit(_) -> () (* WARNING : Pattern Literal is dynamic semantics, not check here *)
    | PatVar(vpat) -> ( match vpat.vpat_typ with
        | None -> add_variable context vpat.vpat_symb vpat.vpat_mut vpat.vpat_name (Some typ)
        | Some(ty) -> 
            if Helper.ty_eq ty typ then 
                add_variable context vpat.vpat_symb vpat.vpat_mut vpat.vpat_name vpat.vpat_typ
            else
                error_type (PatternError(pattern, " declared type doesn't match with the given expr"))
        )
    (* TODO Discuss PatAs *)
    | PatAs(pattern, vpat) -> 
        (* let type inference *)
        walk_pattern context pattern typ;
        ( match vpat.vpat_typ with
        | None -> add_variable context vpat.vpat_symb vpat.vpat_mut vpat.vpat_name (Some typ)
        | Some(ty) -> 
            if ty = typ then 
                add_variable context vpat.vpat_symb vpat.vpat_mut vpat.vpat_name vpat.vpat_typ
            else
                error_type (PatternError(pattern, " declared type doesn't match with the given expr"))
        )
    | PatADT(adt_label, patterns) -> ( match Hashtbl.find_opt table.adt adt_label with
        | None -> error_type (PatternError (pattern, "adt_label " ^adt_label ^ " not found"))
        | Some(data) ->  
            (* WARNING : raise error if length of patterns and that of adt children differ *)
            let walk_iter2 pattern typ = walk_pattern context pattern typ in
            List.iter2 walk_iter2 patterns data.typ
        )
    | PatStruct(type_name, field_patterns)->
        (* Typing rules here *)
        (* walk_iter (struct_field, pattern) *)
        let core = ( match Hashtbl.find_opt table.typ type_name with
        | Some(Struct_data(data)) -> data.core
        | _ -> error_type (PatternError (pattern, "name "^type_name^" is not a struct"))
        ) in
        let walk_iter (struct_field , pattern) = ( 
            match Hashtbl.find_opt core struct_field with
            | None -> error_type (PatternError (pattern, "struct "^type_name^" has no field named "^struct_field))
            | Some(data) -> walk_pattern context pattern data.typ
        ) in
        List.iter walk_iter field_patterns
    | PatTuple(patterns) ->
        (* Typing rules here *)
        let typs = ( match typ.shape with
        | TyTuple(typs) -> typs
        | _ -> error_type (PatternError (pattern, "expr is NOT a Tuple"))
        ) in
        (* WARNING : raise error if length of patterns and that of adt children differ *)
        let walk_iter2 pattern typ = walk_pattern context pattern typ in
        List.iter2 walk_iter2 patterns typs
    ;;

(** Type Check implemented in [walk_expr]: 
    {[ 
    # let ty = walk_expr context expr
      val ty : typ = <...>
    ]} 

    [ty] is the type of expr. 
 *)

let rec walk_expr (context:context) (expr:expr) : typ = 
    let table = context.table in
    match expr.shape with 
    | ExpLit(literal) -> ( match literal with
        | LitUnit      -> Helper.unit
        | LitBool(_)   -> Helper.bool
        | LitInt(_)    -> Helper.i32  (* Discuss *)
        | LitFloat(_)  -> Helper.f32  (* Discuss *)
        | LitChar(_)   -> Helper.char 
        | LitString(s) -> Helper.array Helper.char (String.length s + 1) (* Discuss *)
        )
    | ExpVar(name)-> 
        ( match find_var_opt context name with 
        | Some(symbol) -> 
            Hashtbl.add table.ref expr.expr_id
            { sym = symbol 
            };
            ( match Hashtbl.find_opt table.var symbol with
            | None -> error_type (Error "Impossible, symbol not found, DEBUG needed")
            | Some(data) -> data.typ
            )
        | None -> error_type (Error ("varaible " ^ name ^ " Not Found"))
        )
    | ExpUnOp(op, expr) -> let typ = walk_expr context expr in 
        ( match op with 
        | UnOpNeg -> ( match typ.shape with
            | TyInt(_, _) -> typ
            | TyFloat(_)  -> typ
            | TyChar      -> typ
            | _           -> error_type (Error " apply - to Not a Number ")
        )
        | UnOpNot -> ( match typ.shape with
            | TyBool -> typ
            | _      -> error_type (Error " apply ! to Not a Boolean ")
        ))
    | ExpBinOp(op, left_e, right_e) ->
        let left  = walk_expr context left_e in
        let right = walk_expr context right_e  in
        ( match op with
        | BinOpCompare(op) -> ( match op with
            | BinOpLt | BinOpLeq | BinOpGt | BinOpGeq -> 
                if (Helper.ty_eq left right) then ( match right.shape with
                    | TyInt(_, _) | TyFloat(_) | TyChar      
                        -> Helper.bool
                    | _ -> error_type (ExprError(expr, " ordering values from none of Int, Float, Char "))
                )
                else error_type (ExprError(expr, " ordering values from different types"))
            | BinOpEq | BinOpNeq -> 
                if (Helper.ty_eq left right) then 
                    Helper.bool
                else error_type (ExprError(expr, " equaling values from different types"))
            )
        | BinOpCalculate(op) -> ( match op with
            | BinOpLOr | BinOpLAnd | BinOpLXor -> 
                if (Helper.ty_eq left right) then ( match right.shape with
                    | TyBool      -> Helper.bool
                    | _           -> error_type (ExprError(expr, " logical operate non Bool "))
                )
                else error_type (ExprError(expr, " logical operate non Bool "))
            | BinOpBOr | BinOpBAnd | BinOpBXor ->
                if (Helper.ty_eq left right) then ( match right.shape with 
                    | TyInt(_, _) -> right (* Discuss *)
                    | _           -> error_type (ExprError(expr, " bits operate non Int "))
                )
                else error_type (ExprError(expr, " bits operate non Int "))
            | BinOpLShift | BinOpRShift -> ( match right.shape with 
                | TyInt(_, _) -> ( match left.shape with  (* Discuss *)
                    | TyInt(_, _) -> right (* Discuss *)
                    | _           -> error_type (ExprError(expr, " shifting non Int "))
                )
                | _           -> error_type (ExprError(expr, " shifting a NaN bits "))
                )
            | BinOpAdd | BinOpSub | BinOpMul | BinOpDiv ->
                if (Helper.ty_eq left right) then ( match right.shape with 
                    | TyInt(_, _) | TyFloat(_)  
                        -> right
                    | _ -> error_type (ExprError(expr, " arithmetic on neither Int nor Float "))
                )
                else error_type (ExprError(expr, " arithmetic on different types"))
            | BinOpMod -> 
                if (Helper.ty_eq left right) then ( match right.shape with 
                    | TyInt(_, _) -> right
                    | _           -> error_type (ExprError(expr, " mod on non Int "))
                )
                else error_type (ExprError(expr, " mod on different types"))
            )
        )
    | ExpTuple(exprs) -> Helper.tuple ( List.map (fun expr -> walk_expr context expr ) exprs )
    | ExpADT(label, exprs) -> ( match Hashtbl.find_opt table.adt label with
        | None       -> error_type (Error (" ADT label " ^ label ^ " Not Found "))
        | Some(data) -> (
            if (List.equal Helper.ty_eq data.typ (List.map (fun expr -> walk_expr context expr) exprs)) then
                Helper.named data.sum []
            else
                error_type (Error (" types doesn't match with ADT label "^ label))
        ))
    | ExpStruct(name, field_exprs) -> ( match Hashtbl.find_opt table.typ name with
        | None -> error_type (Error (" type " ^ name ^ " Not Found "))
        | Some(ADT_data(_)) -> error_type (Error (" type " ^ name ^ " is not a Struct but an ADT "))
        | Some(Intf_data(_)) -> error_type (Error (""^name ^ " is not a Struct but an Interface "))
        | Some(Struct_data(data)) -> ( let def_fields = data.fields in
                let map_def_field (def_field:struct_def_field) = match def_field with
                    | (field, typ, _) -> (field, typ)
                in 
                let map_field_expr ((field, expr)) = 
                    (field, walk_expr context expr)
                in
                if List.map map_def_field def_fields = List.map map_field_expr field_exprs then
                    Helper.named name []
                else
                    error_type (Error (" types doesn't match with fields of Struct " ^ name))
        ))
    | ExpField(expr, field) -> let rec find_field (typ:typ) (field:struct_field) : typ option =
        ( match typ.shape with 
        | TyNamed(name, tys) -> 
            unsupport_generics tys;
            ( match Hashtbl.find_opt table.typ name with
            | Some(Struct_data(data)) -> 
                let def_fields = data.fields in
                let find_map def_field = ( match def_field with
                    | (field', typ, _) 
                    -> if field'=field then Some typ else None
                ) in 
                (* find in this level first *)
                ( match List.find_map find_map def_fields with 
                | Some(typ) -> Some(typ)
                (* if failed, try to find it in delegated fields *)
                | None ->  let types = List.filter_map
                    (fun field -> match field with
                        | (_, _, Struct_Field) -> None
                        | (_, typ, Struct_Delegate) -> Some(typ)
                    ) def_fields in 
                    let find_map' (typ:typ) = find_field typ field in
                        List.find_map find_map' types
                )
            | _ -> None
            )
        | _           -> None (* error_type (Error " expr is not a term of struct type") *)
        ) in 
        let typ = walk_expr context expr in
        ( match typ.shape with 
        | TyNamed(name, tys) -> 
            unsupport_generics tys;
            ( match Hashtbl.find_opt table.typ name with
            | None       -> error_type (Error "It's impossible. DEBUG why struct name not found")
            | Some(ADT_data(_)) -> error_type (Error " ADT has no fields! ")
            | Some(Intf_data(_)) -> error_type (Error " Interface has no fields! ")
            | Some(Struct_data(_)) -> ( match find_field typ field with
                | None -> error_type (Error (" field " ^ field ^ " not found "))
                | Some(typ) -> typ
                )
            )
        | _        -> error_type (Error " expr is not a term of struct type")
        )
    | ExpThis -> context.this;
    | ExpApp(func, args) -> (* interface check included *)
        ( match Hashtbl.find_opt table.fnc func with
        | None -> error_type (Error (" function " ^ func ^ " not found "))
        | Some(data) -> 
            (* arg : formal, i.e. declared in function , para : actual *)
            let ty_eq_with_intf (para_t:typ) (arg_t:typ) = 
                if Helper.ty_eq para_t arg_t then true 
                else match arg_t.shape with (* para_t = Type <: Intf = arg_t *)
                | TyNamed(intf_name, tys) -> 
                    unsupport_generics tys;
                    ( match Hashtbl.find_opt table.typ intf_name with
                    | None -> error_type (Error "Impossible, DEBUG please")
                    (* arg_t is a interface *)
                    | Some(Intf_data(_)) -> ( match para_t.shape with
                        | TyNamed(type_name, tys) -> 
                            unsupport_generics tys;
                            ( match Hashtbl.find_opt table.typ type_name with
                            | None -> error_type (Error "Impossible, DEBUG please")
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
            let para_types = List.map (fun expr->walk_expr context expr) args in
            let result = List.for_all2 ty_eq_with_intf para_types data.args in
            if result then data.rety
            else error_type (Error " function args' types doesn't match ")
        )
    
    | ExpMethod(obj, meth, args) -> (* TODO Helper.ty_eq with type This *)
        let typ = walk_expr context obj in
        let name =         
        ( match typ.shape with
        | TyNamed(name, tys) -> 
            unsupport_generics tys;
            name
        | _ -> error_type (Error " Unsupported ") (* Discuss : Support impl for just TyNamed(name, []) currently *)
        ) in
        let methtbl = 
        ( match Hashtbl.find_opt table.typ name with
        | Some(ADT_data(data)) -> data.meth
        | Some(Struct_data(data)) -> data.meth
        | Some(Intf_data(data)) -> data.meth
        | None -> error_type (Error (" method not found for type " ^ name))
        ) in
        let data = ( match Hashtbl.find_opt methtbl meth with
        | Some(data) -> data
        | None -> error_type (Error (" method " ^ meth ^ " not found in type " ^ name))
        ) in
        let para_types = List.map (fun expr->walk_expr context expr) args in 
        let result = List.for_all2 Helper.ty_eq data.args para_types in
        if result then data.rety
        else error_type (Error " method args' types doesn't match ")

    | ExpIf(cond, fst, snd) -> 
        let cond = walk_expr context cond in
        ( match cond.shape with
        | TyBool -> ()
        | _      -> error_type (ExprError(expr, " condition of if is not a bool ") )
        );
        let fst = walk_expr context fst in
        let snd = walk_expr context snd in
        if Helper.ty_eq fst snd then snd 
        else error_type (Error " types of fst and snd of if don't equal ")
    | ExpMatch(expr, pattern_exprs) -> 
        (* Discuss : Only support match term:ADT with ...
         * as for other pattern matches, use let pattern = term:Type
         *)
        let typ = walk_expr context expr in
        ( match typ.shape with
        | TyNamed(name, tys) -> 
            unsupport_generics tys;
            ( match Hashtbl.find_opt table.typ name with
            | None       -> error_type (Error "It's impossible. DEBUG why ADT name not found")
            | Some(Intf_data(_)) -> error_type (Error "cannot match with Interface")
            | Some(Struct_data(_)) -> error_type (Error " cannot match with Struct")
            (* | Some(ADT_data(data)) -> let branches = data.core in *)
            | Some(ADT_data(_)) ->
                (* TODO Discuss : how to solve pattern exhaustive not only here but also StmtMatch *)
                (* Discussion : Temporarily not implement. *)
                (* branches unused because pattern exhaustion hasn't been done *)
                (* check all exprs from pattern_exprs : the same type
                 * then return it
                 *)
                let mapper (pattern, expr) = (
                    scope_beg context;
                    let typ = walk_expr context expr in
                    walk_pattern context pattern typ;
                    scope_end context;
                    typ
                ) in
                let types = List.map mapper pattern_exprs in
                let ty1 = match types with
                    | [] -> error_type (Error " match no branches ")
                    | t::_ -> t 
                in 
                if List.for_all (fun ty'->Helper.ty_eq ty' ty1) types then
                    ty1
                else
                    error_type (Error " match branches return different types");
            ) 
        | _           -> error_type (Error " expr is not a term of ADT")
        (* As for tuple, use let assignment to fetch its sub-fields *)
        )
    ;;

(** walk stmt
    As for return type check, please set [context.rety]
    {[
        walk_stmt context stmt ;;
    ]}
    When meeting a return-stmt, {! walk_stmt} will compare [context.rety] with the type of [expr]
 *)
let rec walk_stmt (context:context) (stmt:stmt) : unit = 
    let table = context.table in
    match stmt.shape with
    | StmtSeq(stmts) -> 
        scope_beg context;
        let walk_iter stmt = walk_stmt context stmt in
        List.iter walk_iter stmts;
        scope_end context;
    | StmtExpr(expr) -> 
        let _ = walk_expr context expr in
        ()
    | StmtAssign(_, left, right) -> let name = 
        (   match left.shape with 
        |   ExpVar(name) -> name
        |   _ -> error_type (StmtError (stmt, " assign to not a variable"))
        ) in
        let symbol = 
        (   match find_var_opt context name with 
        |   Some(symbol) -> symbol
        |   None -> error_type (StmtError (stmt, "varaible " ^ name ^ " Not Found"))
        )   in
        Hashtbl.add table.ref left.expr_id { sym = symbol };
        let data = 
        (   match Hashtbl.find_opt table.var symbol with
            | None -> error_type (StmtError(stmt,  "Impossible, symbol not found, DEBUG needed"))
            | Some(data) -> data
        ) in
        let _ = (* mutability check *)
        (   match data.mut with
        |   Imm -> error_type (StmtError (stmt, " assign to a immutable variable " ^ name))
        |   Mut -> ()
        ) in
        let var_ty = data.typ in
        (* TODO : check for (left op right) *)
        let res_ty = walk_expr context right in
        if Helper.ty_eq var_ty res_ty then ()
        else error_type (StmtError (stmt, " assign a value:T1 to a variable:T2, while T1!=T2"))
    | StmtDecl(pattern, expr) -> 
        let typ = walk_expr context expr in
        walk_pattern context pattern typ
    | StmtIf(cond, t_stmt, f_stmto) ->
        let ty = walk_expr context cond in
        ( match ty.shape with
        | TyBool -> ()
        | _ -> error_type (ExprError (cond, " condition of if-statement is not a boolean"))
        );
        walk_stmt context t_stmt;
        ( match f_stmto with 
        | Some(f_stmt) -> walk_stmt context f_stmt
        | None -> ()
        )
            
    (* REMAIN walk pattern FOR-RANGE *)
    | StmtFor(pattern, expr, body)->
        let typ = walk_expr context expr in
        walk_pattern context pattern typ;
        
        context.checkloop <- context.checkloop + 1;
        walk_stmt context body;
        context.checkloop <- context.checkloop - 1;
    | StmtWhile(cond, body) -> 
        let ty = walk_expr context cond in
        ( match ty.shape with
        | TyBool -> ()
        | _ -> error_type (ExprError (cond, " condition of while-statement is not a boolean"))
        );
        context.checkloop <- context.checkloop + 1;
        walk_stmt context body;
        context.checkloop <- context.checkloop - 1;
    | StmtMatch(expr, branches) ->
        let typ = walk_expr context expr in
        let walk_iter (pattern,body) = (
            walk_pattern context pattern typ;
            walk_stmt context body
        ) in List.iter walk_iter branches
    | StmtReturn(expr) ->
        (* process control here *)
        let typ = walk_expr context expr in
        if Helper.ty_eq typ context.rety then ()
        else error_type (ExprError (expr, " return different types "))
    | StmtBreak ->
        (* process control here *)
        (match context.checkloop with
        | 0 -> error_type (StmtError (stmt, "break statement not within loop or switch"))
        | _ -> ()
        )
    | StmtContinue -> 
        (* process control here *)
        (match context.checkloop with
        | 0 -> error_type (StmtError (stmt, "continue statement not within loop or switch"))
        | _ -> ()
        )
    ;;


(** check func name, and collect type info 
    {[
    # walk_func_decl context name parameters return_type
    ]}
*)
let walk_func_decl (context:context) (name:func_name) (args:func_arg list) (rety:typ) : unit =
    let table = context.table in 
    match Hashtbl.find_opt table.fnc name with
    | Some(_) -> error_type (Error ("function name " ^ name ^ " has been used"))
    | None -> 
        (* check TyNamed existence *)
        let _ = walk_type context rety in
        Hashtbl.add table.fnc name
        { args=List.map 
            (fun (arg:func_arg)->
                (* check TyNamed existence *)
                let _ = walk_type context arg.farg_typ in
                arg.farg_typ
            ) args
        ; rety=rety
        ; name=List.map (fun (arg:func_arg)->arg.farg_name) args
        }
    ;;

(** simply walk function's body inductively  *)
let walk_func_stmt (context:context) (args:func_arg list) (rety:typ) (stmt:stmt) =
    scope_beg context;
    let walk_iter (arg:func_arg) = 
        add_variable context arg.farg_symb Imm arg.farg_name (Some arg.farg_typ)
        in
    List.iter walk_iter args;
    context.rety <- rety;
    walk_stmt context stmt;
    scope_end context
    ;;

(** impl intf for T with methods, check whether intf exists and whether it has such methods*)
let walk_method_intf (context:context) (intf:intf_name option) (typ:typ_name) : unit = 
    let table = context.table in
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
                            List.for_all2 Helper.ty_eq fun_data.args fun_data'.args 
                         && Helper.ty_eq fun_data.rety fun_data'.rety
                        )
                    | Some(Struct_data(data)) -> ( match Hashtbl.find_opt data.meth fun_name with
                        | None -> false
                        | Some(fun_data') -> 
                            List.for_all2 Helper.ty_eq fun_data.args fun_data'.args 
                         && Helper.ty_eq fun_data.rety fun_data'.rety
                        )
                    | _ -> error_type (Error " impl type NOT FOUND ")
                else false in
            let result = Hashtbl.fold folder data.meth true in 
            if result then ()
            else error_type (Error " not match with interface's methods ")
        | Some(ADT_data(_)) -> error_type (Error (name ^ " is not an interface but an ADT"))
        | Some(Struct_data(_)) -> error_type (Error (name ^ " is not an interface but a Struct"))
        | None -> error_type (Error ("no interface name as " ^ name))
    )
    ;;

(** similar to {!val-walk_func_decl}, but checked in the scope of type T*)
let walk_method_decl (context:context) (typ:typ_name) (name:func_name) (args:func_arg list) (rety:typ) : unit =
    let table = context.table in
    let fun_table = match Hashtbl.find_opt table.typ typ with
        | None -> error_type (Error ("no type named" ^ typ))
        | Some(Intf_data(_)) -> error_type (Error (typ^" is not a type but an interface") )
        | Some(Struct_data(table)) -> table.meth
        | Some(ADT_data(table)) -> table.meth
    in
    match Hashtbl.find_opt fun_table name with
    | Some(_) -> error_type (Error ("member function name " ^ name ^ " has been used"))
    | None -> 
        (* check TyNamed existence *)
        let _ = walk_type context rety in
        Hashtbl.add fun_table name
        { args=List.map
            (fun (arg:func_arg)->
                (* check TyNamed existence *)
                let _ = walk_type context arg.farg_typ in
                arg.farg_typ
            ) args
        ; rety=rety
        ; name=List.map (fun (arg:func_arg)->arg.farg_name) args
        }
    ;;

let walk_method_stmt = walk_func_stmt;;

let walk_top (context:context) (clause:top_clause) : unit = 
    let table = context.table in
    let nametbl = context.nametbl in
    match clause.shape with 
    | GlobalVarDef(gvar) -> 
        let symbol = gvar.gvar_id in 
        let name = gvar.gvar_name in 
        let typ = walk_expr context gvar.gvar_value in
        (* Global -> context.scope = [] *)
        ( match Hashtbl.find_opt nametbl name with
        | Some(_) -> error_type (Error "The same global variable name")
        | None -> ()
        );
        Hashtbl.add nametbl name symbol;
        Hashtbl.add table.var symbol
            { mut  = Mut
            ; typ  = typ
            ; name = name
            }
    
    | StructDef(def) ->
        let name = def.struct_name in 
        ( match Hashtbl.find_opt table.typ name with
        | Some(_) -> error_type (Error "The same Struct Name")
        | None -> ()
        );
        let core = Hashtbl.create 10 in
        let fields = def.struct_fields in
        let insert (key, typ, attr) = 
            (* check TyNamed existence *)
            let _ = walk_type context typ in
            Hashtbl.add core key {typ=typ; attr=attr}
        in
        List.iter insert fields;
        Hashtbl.add table.typ name 
            ( Struct_data(
            { intf = []
            ; meth = Hashtbl.create 10
            ; core = core
            ; fields = fields
            }))

    | ADTDef(def) ->
        let name = def.adt_name in 
        ( match Hashtbl.find_opt table.typ name with
        | Some(_) -> error_type (Error "The same ADT Name")
        | None    -> ()
        );
        let branches = def.adt_branches in
        let walk_iter index (label, typs) = 
        ( match Hashtbl.find_opt table.adt label with
        | Some(_) -> error_type (Error "The same ADT label")
        | None    -> 
            (* check TyNamed existence *)
            let _ = walk_type context (Helper.tuple typs) in
            Hashtbl.add table.adt label
            { sum = name
            ; typ = typs
            ; tag = index
            }
        ) in
        List.iteri walk_iter branches;
        Hashtbl.add table.typ name (ADT_data(
            { intf = []
            ; meth = Hashtbl.create 10
            ; core = branches
            }))

    | InterfaceDecl(decl) -> 
        let name = decl.intf_decl_name in
        ( match Hashtbl.find_opt table.typ name with
        | Some(_) -> error_type (Error "The same Interface Name")
        | None -> ()
        );
        let fun_table = Hashtbl.create 10 in
        let walk_iter (decl:func_decl) = 
            let args = List.map (fun farg->farg.farg_typ) decl.func_decl_args in
            let name = List.map (fun farg->farg.farg_name) decl.func_decl_args in
            Hashtbl.add fun_table decl.func_decl_name
            { args = args
            ; rety = decl.func_decl_rety
            ; name = name
            } in 
        List.iter walk_iter decl.intf_decl_methods;
        Hashtbl.add table.typ name (Intf_data { meth = fun_table })

    | FunctionDef(func_impl) ->
        let (decl, stmt) = func_impl in
        let args = decl.func_decl_args in
        let name = decl.func_decl_name in
        let rety = decl.func_decl_rety in
        walk_func_decl context name args rety; (* func name check, modify table.fnc *)
        context.rety <- rety;
        walk_func_stmt context args rety stmt (* return type check *)

    | MethodsImpl(impl) -> 
        (* Typing rules for MethodsImpl here*)
        let typ = impl.impl_typ in
        let methods = impl.impl_methods in
        let intf = impl.impl_intf in

        let walk_iter ((decl, stmt):func_impl) =
            let args = decl.func_decl_args in
            let name = decl.func_decl_name in
            let rety = decl.func_decl_rety in
            walk_method_decl context typ name args rety;
            context.this <- Helper.named typ [];
            walk_method_stmt context args rety stmt
        in
        List.iter walk_iter methods;
        walk_method_intf context intf typ
    ;;

(** {1 Usage} *)

(** See [WalkerTest.ml] . *)
