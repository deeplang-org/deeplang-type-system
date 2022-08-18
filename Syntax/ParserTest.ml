
open Syntax

let parse_file file =
    let ch = open_in file in
    let lexbuf = Lexing.from_channel ch in
    Lexing.set_filename lexbuf file;
    let program = Parser.program Lexer.token lexbuf in
    program

let test_valid path =
    Format.printf "@[<v>";
    try ignore (parse_file path);
        true
    with SyntaxError.Error(span, err) ->
        Format.printf "syntax error: %a@ in %a\n"
            SyntaxError.pp_error err SyntaxError.pp_span span;
        false

let test_invalid path =
    Format.printf "@[<v>";
    try ignore (parse_file path);
        Format.printf "%s should have a syntax error!\n" path;
        false
    with SyntaxError.Error(span, err) ->
        Format.printf "expected syntax error: %a@ in %a\n"
            SyntaxError.pp_error err SyntaxError.pp_span span;
        true

let _ =
    Format.printf "@[<v>";
    if List.for_all Fun.id (List.map test_valid ["test/parser.dp"; "test/types.dp"] 
        @ List.map test_invalid [ "test/expressions/cons_no_args.dp"
                                ; "test/expressions/bad_struct_field.dp"
                                ; "test/expressions/bad_struct_field2.dp"
                                ; "test/expressions/bad_unary_op.dp"
                                ; "test/expressions/bad_unary_op2.dp"
                                ; "test/expressions/bad_unary_op3.dp"
                                ; "test/expressions/bad_field_access.dp"
                                ; "test/expressions/bad_field_access2.dp"
                                ; "test/expressions/bad_method_access.dp"
                                ; "test/expressions/bad_method_access2.dp"
                                ; "test/expressions/bad_binary_op.dp"
                                ; "test/expressions/bad_binary_op2.dp"
                                ; "test/expressions/mismatched_parens.dp"
                                ; "test/expressions/mismatched_parens2.dp"
                                ; "test/expressions/mismatched_parens3.dp"
                                ; "test/expressions/mismatched_brack.dp" ])
        then (Format.printf "OK!\n"; exit 0)
        else (Format.printf "Not OK!\n"; exit 1)
