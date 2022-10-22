
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
                                ; "test/expressions/mismatched_brack.dp"
                                ; "test/patterns/bad_match_branch1.dp"
                                ; "test/patterns/bad_match_branch2.dp" 
                                ; "test/patterns/bad_pattern_ADT.dp" 
                                ; "test/patterns/bad_pattern_struct1.dp"
                                ; "test/patterns/bad_pattern_struct2.dp"
                                ; "test/patterns/bad_variable_pattern1.dp"
                                ; "test/patterns/bad_variable_pattern2.dp"
                                ; "test/patterns/multiple_patterns.dp"
                                ; "test/patterns/unclosed_brace1.dp"
                                ; "test/patterns/unclosed_brace2.dp"
                                ; "test/patterns/unclosed_paren1.dp"
                                ; "test/patterns/unclosed_paren2.dp"
                                ; "test/statements/missing_if1.dp"
                                ; "test/statements/missing_if2.dp"
                                ; "test/statements/missing_semicolon1.dp"
                                ; "test/statements/missing_semicolon2.dp"
                                ; "test/statements/missing_semicolon3.dp"
                                ; "test/statements/missing_semicolon4.dp"
                                ; "test/statements/missing_semicolon5.dp"
                                ; "test/statements/missing_semicolon6.dp"
                                ; "test/statements/bad_assignment1.dp"
                                ; "test/statements/bad_assignment2.dp"
                                ; "test/statements/no_exp1.dp"
                                ; "test/statements/no_exp2.dp"
                                ; "test/statements/no_exp3.dp"
                                ; "test/statements/no_exp4.dp"
                                ; "test/statements/missing_Lparen1.dp"
                                ; "test/statements/missing_Lparen2.dp"
                                ; "test/statements/missing_Lparen3.dp"
                                ; "test/statements/missing_Rparen1.dp"
                                ; "test/statements/missing_Rparen2.dp"
                                ; "test/statements/missing_Rparen3.dp"
                                ; "test/statements/missing_Rbrace.dp"
                                ; "test/statements/bad_for1.dp"
                                ; "test/statements/bad_for2.dp"
                                ; "test/statements/bad_let1.dp"
                                ; "test/statements/bad_let2.dp"
                                ; "test/others/missing_type.dp"
                                ; "test/others/missing_type2.dp"
                                ; "test/others/missing_type3.dp"
                                ; "test/others/missing_type4.dp"
                                ; "test/others/missing_type5.dp"
                                ; "test/others/missing_type6.dp"
                                ; "test/others/missing_function_decl_arg.dp"
                                ; "test/others/bad_adt_branch_name.dp"
                                ; "test/others/missing_function_decl_arg.dp"
                                ; "test/others/bad_adt_branch_name.dp"
                                ; "test/others/function_decl_args_without_comma.dp"
                                ; "test/others/mutiple_type.dp"
                                ; "test/others/mutiple_type_mut.dp"
                                ; "test/others/unknown_type.dp"
                                ; "test/others/func_impl_without_rbrace.dp"
                                ; "test/others/bad_interface_name.dp"
                                ])
        then (Format.printf "OK!\n"; exit 0)
        else (Format.printf "Not OK!\n"; exit 1)
