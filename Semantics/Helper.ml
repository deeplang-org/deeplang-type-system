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

let unit : typ =
    { shape = TyUnit
    ; span  = span_dummy
    };;
let bool : typ = 
    { shape = TyBool
    ; span  = span_dummy
    };;
let i32 : typ = 
    { shape = TyInt(Signed,ISize_32)
    ; span  = span_dummy
    };;
let f32 : typ =
    { shape = TyFloat(FSize_32)
    ; span  = span_dummy
    };;
let char : typ =
    { shape = TyChar
    ; span  = span_dummy
    };;
let array (typ:typ) (size:int) : typ =
    { shape = TyArray(typ, size)
    ; span  = span_dummy
    };;
let tuple (typs:typ list) : typ =
    { shape = TyTuple(typs)
    ; span  = span_dummy
    };;
let var (name:typ_name) : typ =
    { shape = TyVar(name)
    ; span  = span_dummy (* Discuss : maybe not*)
    };;
let this : typ =
    { shape = TyThis
    ; span  = span_dummy
    };;