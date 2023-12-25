let parse_file file : Syntax.ParseTree.top_clause list = 
  let ch = open_in file in
  let lexbuf = Lexing.from_channel ch in
  Lexing.set_filename lexbuf file;
  let program = Syntax.Parser.program Syntax.Lexer.token lexbuf in
  program
  ;;

open Semantics.Walker
open Semantics.Table

let process_file file =
  let table : table = 
    { var=Hashtbl.create 10
    ; fnc=Hashtbl.create 10
    ; typ=Hashtbl.create 10
    ; adt=Hashtbl.create 10
    ; ref=Hashtbl.create 10
    }
  in

  let context : context = 
    { table   = table
    ; nametbl = Hashtbl.create 10
    ; scope   = []
    ; this    = Semantics.Helper.unit
    ; rety    = Semantics.Helper.unit
    ; checkloop = 0
    }
  in

  let ast =
    try parse_file file with Syntax.SyntaxError.Error(span, err) ->
      Format.printf "syntax error: %a@ in %a@ "
          Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
      []
  in

  ast |> List.iter (fun clause ->
    try walk_top context clause with Semantics.SemanticsError.ErrorType(err) ->
      Format.printf "semantics error: %a@ "
        Semantics.SemanticsError.print_error err);

  ast |> List.iter (fun x ->
    Format.printf "%a@ "
      IR.ANF.pp_function_definition
      (IR.Conversion.trans_top_clause [] x))

let _ =
  Format.printf "@[<v>";
  for i = 1 to Array.length Sys.argv - 1 do
    process_file Sys.argv.(i);
    Format.printf "@ "
  done;
  Format.printf "@]";
