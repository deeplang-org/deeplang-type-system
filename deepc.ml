(* deepc.ml — Deeplang compiler frontend.

   Compiles Deeplang source (.dp) through the full pipeline:
   Source (.dp) → Lexer → Parser → Semantic Walker → ANF Conversion → WAT.

   Usage: deepc [--anf] <file.dp> ...
   By default writes a <file>.wat (WebAssembly Text Format) next to each
   input. With --anf, writes a <file>.anf (A-Normal Form) instead. *)

let version = "v1.0.0"
let target = "WebAssembly 1.0"

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

let output_path file ext =
  let f = normalize_filename file in
  if Filename.check_suffix f ".dp" then
    Filename.chop_suffix f ".dp" ^ ext
  else
    f ^ ext

let compile_file file : string * string =
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
  let wat = IR.WasmGen.generate_wat context.table program in
  let anf =
    Format.asprintf "@[<v>%a@]"
      (Format.pp_print_list
         (fun fmt fd -> Format.fprintf fmt "%a@ " IR.ANF.pp_function_definition fd))
      program
  in
  (wat, anf)

let compile_file_to anf_only file =
  let wat, anf = compile_file file in
  let content, out =
    if anf_only then (anf, output_path file ".anf")
    else (wat, output_path file ".wat")
  in
  let oc = open_out out in
  output_string oc content;
  output_char oc '\n';
  close_out oc;
  Format.printf "compiled %s -> %s@." (normalize_filename file) out

let () =
  let argc = Array.length Sys.argv in
  if argc >= 2 && (Sys.argv.(1) = "--version" || Sys.argv.(1) = "-v") then begin
    Format.printf "deepc %s@." version;
    Format.printf "  supports %s@." target;
    exit 0
  end;
  let anf_only = argc >= 2 && Sys.argv.(1) = "--anf" in
  let first_file = if anf_only then 2 else 1 in
  if first_file >= argc then begin
    Format.eprintf "usage: %s [--anf] <file.dp> ...@." Sys.argv.(0);
    exit 2
  end;
  let files = Array.sub Sys.argv first_file (argc - first_file) in
  Array.iter (compile_file_to anf_only) files
