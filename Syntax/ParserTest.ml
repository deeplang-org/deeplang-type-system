
open Syntax

let parse_file file =
    let ch = open_in file in
    let lexbuf = Lexing.from_channel ch in
    Lexing.set_filename lexbuf file;
    let program = Parser.program Lexer.token lexbuf in
    program

let _ =
    Format.printf "@[<v>";
    try ignore (parse_file "test/parser.dp")
    with SyntaxError.Error(span, err) ->
        Format.printf "syntax error: %a@ in %a"
            SyntaxError.pp_error err SyntaxError.pp_span span;
        exit 1
