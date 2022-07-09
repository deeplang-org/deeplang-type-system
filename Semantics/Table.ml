open Syntax.ParseTree;;
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
    ; rety  : typ
    ; name  : variable list (* args' names *)
    }
    ;;
type fun_table = (func_name, fun_data) Hashtbl.t;;

type adty_data = 
    { mutable intf : intf_name list (* impl interface for T *)
    ; mutable meth : fun_table      (* impl for T *)
    ;         core : (adt_label * typ list) list
    };;
type field_data = 
    { typ : typ
    ; attr : struct_def_field_attr
    };;
type struct_data = 
    { mutable intf : intf_name list
    ; mutable meth : fun_table
    ;         core : (struct_field, field_data) Hashtbl.t 
    ;       fields : struct_def_field list
    };;
type intf_data = 
    {         meth : fun_table
    };;
type typ_data =
    | ADT_data of adty_data
    | Struct_data of struct_data
    | Intf_data of intf_data
    ;;
type typ_table = (typ_name, typ_data) Hashtbl.t;; (* T -> impl what intf and what method *)
type adt_data = 
    { sum : typ_name (** sum_type's name *)
    ; typ : typ list (** parameters of constructor *)
    }
    ;;
type adt_table = (adt_label, adt_data) Hashtbl.t;;

type ref_data = 
    { sym : symbol (** point to the varaible *)
    }
    ;;
type ref_table = (NodeId.expr, ref_data ) Hashtbl.t ;;

type table = 
    { var : var_table (** the mutability, type and name of variable *)
    ; fnc : fun_table (** the signature/type of function *)
    ; typ : typ_table (** the info of adt, struct and interface *)
    ; adt : adt_table (** the signature/type of adt_labels/constructor  *)
    ; ref : ref_table (** the use of variable *)
    };;