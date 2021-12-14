
let id size = 
    let time = Int.of_float (Unix.time()) in
    let rint = (Random.int size) in
    time * size + rint
;;

(* let sym_id_gen () = id 1000;; *)
let sym_id_gen () = 0;
(* sym_id generator with Unix.time and Random.int *)

module Dcl_Tab = Map.Make(Int);; 
type dcl_tab_ent = 
    { mutable name: string
    ; mutable scope_ref : dcl_tab_ent Dcl_Tab.t
    ; scope_level : int
    }
;;
let print_key int dte = print_string ( (Int.to_string int) ^ "\n" );;

let rec tableCode (e:AbsDeeplang.code) : dcl_tab_ent Dcl_Tab.t = match e with
       AbsDeeplang.Declare declare -> tableDeclare declare 
  |    AbsDeeplang.Define define -> tableDefine define
  |    AbsDeeplang.Statement statement -> tableStatement statement
  |    AbsDeeplang.Expression expression -> tableExpression expression
  |    AbsDeeplang.Unit  -> Dcl_Tab.empty


and varId2String (varid:AbsDeeplang.varId) = match varid with
    AbsDeeplang.VarId string -> string
and tableDeclare (e:AbsDeeplang.declare) : dcl_tab_ent Dcl_Tab.t = match e with
        AbsDeeplang.DecImmut (varid, type') -> Dcl_Tab.add (sym_id_gen()) {
           name=(varId2String varid) 
        ;  scope_ref=Dcl_Tab.empty
        ;  scope_level=1
        } Dcl_Tab.empty
  |    AbsDeeplang.DecMut (varid, type') -> Dcl_Tab.add (sym_id_gen()) {
           name=(varId2String varid) 
        ;  scope_ref=Dcl_Tab.empty
        ;  scope_level=1
        } Dcl_Tab.empty
  |    AbsDeeplang.DecFunc (varid, args, rettype) -> Dcl_Tab.empty
  |    AbsDeeplang.Interface (interfacename, methods) -> Dcl_Tab.empty

and tableDefine (e:AbsDeeplang.define) = Dcl_Tab.empty



and tableStatement (e:AbsDeeplang.statement) = Dcl_Tab.empty



and tableExpression (e:AbsDeeplang.expression) = Dcl_Tab.empty
;;

let node : AbsDeeplang.code = 
AbsDeeplang.Declare 
    (AbsDeeplang.DecImmut(
        AbsDeeplang.VarId "x",
        AbsDeeplang.TypeI32
    ));;
let m = tableCode node;;

Dcl_Tab.find 0 m;;