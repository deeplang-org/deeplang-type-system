open Syntax.ParseTree;;

(* walker中的错误类型，目前抛出的错误为以下类型中的一种 *)
type error_walker = 
    | Error             of string 
    | TypeError         of typ      * string
    | PatternError      of pattern  * string
    | ExprError         of expr     * string
    | StmtError         of stmt     * string
    | TopError          of top_clause * string

exception ErrorType of error_walker

(* 实际调用抛出错误的函数，在使用时可以直接调用 *)
let error_type (error_message:error_walker) = 
    raise (ErrorType(error_message))

(* @todo 这部分是将来期望能够加入位置信息的报错代码，暂时还没有用到 *)
(* @todo 未来期望能够在报错信息中加入位置信息，预留出的位置 *)
open Syntax.SyntaxError;;

(* exception ErrorType of src_span * error_walker *)

(* 实际调用抛出错误的函数，在使用时可以直接调用 *)
(* let error_type (error_message:error_walker) = 
    raise (ErrorType((cur_span ()), error_message)) *)


(* 以下内容为打印调试信息的代码，可不看 *)
[@@@warning "-27"]
let print_error fmt (err:error_walker) =
    let open Format in
    match err with
    | Error msg         
        -> fprintf fmt "[Error] %s\n"   msg
    | TypeError (typ, msg) 
        -> fprintf fmt "[Type Error] %s@ in %a\n"   msg pp_span typ.span
    | PatternError (pattern, msg) 
        -> fprintf fmt "[Pattern Error] %s@ in %a\n" msg pp_span pattern.span
    | ExprError (expr, msg) 
        -> fprintf fmt "[Expression Error] %s@ in %a\n"  msg pp_span expr.span
    | StmtError (stmt, msg) 
        -> fprintf fmt "[Statement Error] %s@ in %a\n"  msg pp_span stmt.span
    | TopError (top_clause, msg)
        -> fprintf fmt "[Top Error] %s@ in %a\n"  msg pp_span top_clause.span

(* expression 的 pretty printer，暂时还没有写出来，ww *)
(* let print_expr expr =
    pp_expr std_formatter expr;
    pp_print_flush std_formatter () *)