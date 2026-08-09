(** End-to-end test: parse .dp file, walk semantics, convert to ANF, generate WAT *)

let normalize_filename file =
  let len = String.length file in
  if len >= 2 && String.sub file 0 2 = "./" then
    String.sub file 2 (len - 2)
  else
    file

let parse_file file : Syntax.ParseTree.top_clause list =
  let ch = open_in file in
  let lexbuf = Lexing.from_channel ch in
  Lexing.set_filename lexbuf (normalize_filename file);
  let program = Syntax.Parser.program Syntax.Lexer.token lexbuf in
  program

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

  IR.ANF.reset_generator ();

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

  let fname = normalize_filename file in

  let program = IR.Conversion.trans_program ~table ast in

  (* First print ANF (same as ConversionTest) *)
  Format.printf "=== ANF for %s ===@ " fname;
  program |> List.iter (fun fd ->
    Format.printf "%a@ " IR.ANF.pp_function_definition fd);

  (* Then generate and print WAT *)
  Format.printf "=== WAT for %s ===@ " fname;
  let wat = IR.WasmGen.generate_wat table program in
  Format.printf "%s@ " wat

let _ =
  Format.printf "@[<v>";
  let files = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  Array.sort String.compare files;
  Array.iter (fun file ->
    process_file file;
    Format.printf "@ ") files;
  Format.printf "@]";
