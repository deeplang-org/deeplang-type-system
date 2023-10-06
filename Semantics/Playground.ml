module OrderedString = struct
  type t = string
  let compare = Stdlib.compare
end

module StringMap = Map.Make(OrderedString)

type top_clause = Syntax.ParseTree.top_clause
type func_impl = Syntax.ParseTree.func_impl
type func_arg = Syntax.ParseTree.func_arg
type context = Semantics.Walker.context
type int_typ_sign = Syntax.ParseTree.int_typ_sign

let trans_func_impl (_context:context) (func_impl:func_impl): Semantics.ANF.function_definition =
  let get_arg_symbol (func_arg:func_arg) = match func_arg.farg_symb with
    Symbol(symbol) -> symbol in
  let (func_decl, stmt) = func_impl in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params = List.map get_arg_symbol func_decl.func_decl_args;
      func_label = Semantics.ANF.gen_label();
      func_body = Return(stmt.span, Val(Int(114514)));
    };;

let trans_top_clause (context:context) (top_clause:top_clause) : Semantics.ANF.function_definition = match top_clause.shape with
  | FunctionDef(func_impl) -> trans_func_impl(context)(func_impl)
  | _ -> failwith "TODO";;

let parse_file file : Syntax.ParseTree.top_clause list = 
  let ch = open_in file in
  let lexbuf = Lexing.from_channel ch in
  Lexing.set_filename lexbuf file;
  let program = Syntax.Parser.program Syntax.Lexer.token lexbuf in
  program
  ;;

open Semantics.Walker
open Semantics.Table

let table : table = 
  { var=Hashtbl.create 10
  ; fnc=Hashtbl.create 10
  ; typ=Hashtbl.create 10
  ; adt=Hashtbl.create 10
  ; ref=Hashtbl.create 10
  }
  ;;

let context : context = 
  { table   = table
  ; nametbl = Hashtbl.create 10
  ; scope   = []
  ; this    = Semantics.Helper.unit
  ; rety    = Semantics.Helper.unit
  ; checkloop = 0
  }
  ;;
let walk = walk_top context;;

let clauses file = 
  try parse_file file with
      Syntax.SyntaxError.Error(span, err) ->
          Format.printf "syntax error: %a@ in %a\n"
              Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
          []
      ;;

let iterator clause = 
  try walk clause with
      Semantics.SemanticsError.ErrorType(err) ->
          Format.printf "semantics error: %a\n"
          Semantics.SemanticsError.print_error err
      ;;

let _ = Format.printf "1919810\n";;




let file = "../examples/basicMain.dp";;

let ast = clauses file;;

let _ = List.iter iterator ast;;

let _ = List.map (fun x -> print_endline (Semantics.ANF.show_function_definition(trans_top_clause(context)(x)))) ast;;
