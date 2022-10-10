(** check TyNamed whether in context.typ *)
open Syntax.ParseTree;;
open Context;;

let unsupport_generics (tys:typ list) = 
    ( match tys with 
    | [] -> () 
    | _::_ -> failwith("Unsupport for Generics Programming")
    )
    ;;

let rec check (context:context) (typ:typ) : bool =
    let table = context.table.typ in
    match typ.shape with 
    | TyNamed(name, tys) -> 
        unsupport_generics tys;
        ( match Hashtbl.find_opt table name with
        | None -> failwith("Type Named "^name^" Not Found")
        | Some(_) -> true
        )
    | TyVar(_) -> failwith("Unsupport for Generics Programming")
    | TyArray(typ, _) -> check context typ
    | TyTuple(tys) -> List.for_all (check context) tys
    | _ -> true
    ;;



