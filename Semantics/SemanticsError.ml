open Syntax.ParseTree;;

(* @todo 未来期望能够在报错信息中加入位置信息，预留出的位置 *)
(* type src_span =
    { span_start : Lexing.position
    ; span_end   : Lexing.position } *)

(* walker中的错误类型，目前抛出的错误为以下类型中的一种 *)
type error_walker = 
    | TypeError         of string 
    | UndefinedOperator of string
    | MismatchParameter of expr * string
    | UnsuitablePattern of pattern * string

exception ErrorType of error_walker

(* 实际调用抛出错误的函数，在使用时可以直接调用 *)
let error_type (error_message:error_walker) = 
    raise (ErrorType(error_message))

(* @todo 这部分是将来期望能够加入位置信息的报错代码，暂时还没有用到 *)
(* exception ErrorWalker of src_span * error_walker *)

(* let error_type_ left right error_message = 
    raise(ErrorWalker({
        span_start = Parsing.rhs_start_pos left;
        span_end = Parsing.rhs_end_pos right
    }, error_message)) *)

(* 以下内容为打印调试信息的代码，可不看 *)
[@@@warning "-27"]
let print_error fmt (err:error_walker) =
    let open Format in
    match err with
    | TypeError msg         -> fprintf fmt "[Type Error] %s\n"   msg
    | UndefinedOperator msg -> fprintf fmt "[Undefined Operator] %s\n"   msg
    | MismatchParameter (expression, msg) -> fprintf fmt "[Mismatched Parameter] %s\n"  msg
    | UnsuitablePattern (pattern, msg) -> fprintf fmt "[Unsuitable Pattern] %s\n" msg

(* expression 的 pretty printer，暂时还没有写出来，ww *)
(* let print_expr expr =
    pp_expr std_formatter expr;
    pp_print_flush std_formatter () *)