(* deepc.ml — Deeplang compiler driver.

   Compiles Deeplang source (.dp) through the full pipeline:
   Source (.dp) → Lexer → Parser → Semantic Walker → ANF Conversion → WAT.

   Usage: deepc <file.dp> ...
   Writes a <file>.wat (WebAssembly Text Format) next to each input. *)

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
  Syntax.Parser.program Syntax.Lexer.token lexbuf

open Semantics.Walker
open Semantics.Table

let make_context () : context =
  let table : table =
    { var = Hashtbl.create 10
    ; fnc = Hashtbl.create 10
    ; typ = Hashtbl.create 10
    ; adt = Hashtbl.create 10
    ; ref = Hashtbl.create 10
    }
  in
  { table
  ; nametbl = Hashtbl.create 10
  ; scope   = []
  ; this    = Semantics.Helper.unit
  ; rety    = Semantics.Helper.unit
  ; checkloop = 0
  }

let output_path file =
  let f = normalize_filename file in
  if Filename.check_suffix f ".dp" then
    Filename.chop_suffix f ".dp" ^ ".wat"
  else
    f ^ ".wat"

let compile_file file : string =
  let context = make_context () in
  IR.ANF.reset_generator ();

  let ast =
    try parse_file file with
    | Syntax.SyntaxError.Error (span, err) ->
        Format.eprintf "syntax error: %a@ in %a@."
          Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
        exit 1
  in

  List.iter
    (fun clause ->
      try walk_top context clause with
      | Semantics.SemanticsError.ErrorType err ->
          Format.eprintf "semantics error: %a@."
            Semantics.SemanticsError.print_error err;
          exit 1)
    ast;

  let program = IR.Conversion.trans_program ~table:context.table ast in
  IR.WasmGen.generate_wat context.table program

let compile_file_to file =
  let wat = compile_file file in
  let out = output_path file in
  let oc = open_out out in
  output_string oc wat;
  output_char oc '\n';
  close_out oc;
  Format.printf "compiled %s -> %s@." (normalize_filename file) out

let () =
  if Array.length Sys.argv < 2 then begin
    Format.eprintf "usage: %s <file.dp> ...@." Sys.argv.(0);
    exit 2
  end;
  let files = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  Array.iter compile_file_to files
