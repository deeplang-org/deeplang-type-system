
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
    }
    ;;
let walk = walk_top context;;

let rec test_walk_valid pathes = 
    match pathes with
    | path :: rest_path -> (
        try parse_file "test/type.dp" with
        Syntax.SyntaxError.Error(span, err) ->
            Format.printf "syntax error: %a@ in %a"
                Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
            exit 1
        test_walk_valid rest_path
    )
    | [] -> true;;

let clauses = try parse_file "test/type.dp" with
    Syntax.SyntaxError.Error(span, err) ->
        Format.printf "syntax error: %a@ in %a"
            Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
        exit 1
    ;;

let iterator clause = walk clause

let _ = List.iter iterator clauses;;

open Semantics.Helper;;
pp_var_table Format.std_formatter table.var;;