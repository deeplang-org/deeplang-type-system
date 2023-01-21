
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

(* let rec test_walk_valid pathes = 
    match pathes with
    | path :: rest_path -> (
        try parse_file "test/type.dp" with
        Syntax.SyntaxError.Error(span, err) ->
            Format.printf "syntax error: %a@ in %a"
                Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
            exit 1
        test_walk_valid rest_path
    )
    | [] -> true;; *)

(* open SemanticsError;; *)
let rec clauses_file file_list = 
    match file_list with
    | []    -> exit 1
    | file::tails   -> (
        try parse_file file with
            Semantics.SemanticsError.ErrorType(_, msg) ->
                Format.printf "semantics error: %a"
                Semantics.SemanticsError.print_error msg;
                (* @todo 未来加入定位报错时这里也需要修改 *)
                (* Format.printf "semantics error: %a@ in %a"
                Semantics.SemanticsError.pretty_print_error err 
                    Syntax.SyntaxError.pp_span span; *)
        clauses_file tails
    )
    
let clauses = clauses_file ["test/expression.dp"]

(* let clauses = try parse_file "test/expression.dp" with
    Syntax.SyntaxError.Error(span, err) ->
        Format.printf "syntax error: %a@ in %a"
            Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
        exit 1
    ;; *)

let iterator clause = walk clause

let _ = List.iter iterator clauses;;

open Semantics.Helper;;
pp_var_table Format.std_formatter table.var;;