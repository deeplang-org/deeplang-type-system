open Syntax.ParseTree
open Semantics.Walker
let mk_id = let counter = ref 0 in
    fun () -> incr counter; !counter
    ;;
let test_span = BuiltinType._span ;;
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

let context : context = 
    { name_map=NameMap.empty
    };;
let table : table = 
    { var=Hashtbl.create 10
    ; fnc=Hashtbl.create 10
    ; typ=Hashtbl.create 10
    ; adt=Hashtbl.create 10
    ; ref=Hashtbl.create 10
    };;
walk context table test_case;;
table;;