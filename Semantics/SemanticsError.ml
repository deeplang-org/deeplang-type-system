(* open Syntax.ParseTree;;

type src_span =
    { span_start : Lexing.position
    ; span_end   : Lexing.position }

type error_walker = 
    | UndefinedOperator of string
    | MismatchParameter of string
    | UnsuitablePattern of string

(* exception ErrorType of expr * error_walker *)
exception ErrorWalker of src_span * error_walker

let error_type_ left right error_message = 
    raise(ErrorWalker({
        span_start = Parsing.rhs_start_pos left;
        span_end = Parsing.rhs_end_pos right
    }, error_message)) *)