
open Syntax

let parse_file file =
    let ch = open_in file in
    let lexbuf = Lexing.from_channel ch in
    Lexing.set_filename lexbuf file;
    let program = Parser.program Lexer.token lexbuf in
    program

let test_one path =
    Format.printf "@[<v>";
    try ignore (parse_file path);
        true
    with SyntaxError.Error(span, err) ->
        Format.printf "syntax error: %a@ in %a"
            SyntaxError.pp_error err SyntaxError.pp_span span;
        false

let _ =
    Format.printf "@[<v>";
    if List.for_all Fun.id (List.map test_one ["test/parser.dp"; "test/types.dp"])
        then (Format.printf "OK!\n"; exit 0)
        else exit 1
 
