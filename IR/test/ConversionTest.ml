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

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i) in
    let ast = clauses file in
    List.iter iterator ast;
    ast |> List.iter (fun x ->
      print_endline
        (IR.ANF.show_function_definition
            (IR.Conversion.trans_top_clause [] x)));
  done
