
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
    ; checkloop = 0
    }
    ;;
let walk = walk_top context;;

let clauses file = 
    try parse_file file with
        Syntax.SyntaxError.Error(span, err) ->
            Format.printf "syntax error: %a@ in %a\n"
                Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
            []
        ;;

let iterator clause = 
    try walk clause with
        Semantics.SemanticsError.ErrorType(err) ->
            Format.printf "semantics error: %a\n"
            Semantics.SemanticsError.print_error err
        ;;

let file_list = ["test/type.dp"
                ;"test/test.dp"
                ;"test/type/unsupport_type1.dp"
                ;"test/type/unsupport_type2.dp"
                ;"test/type/used_var1.dp"
                ;"test/type/used_var2.dp"
                ;"test/type/used_var3.dp"
                ;"test/type/used_var4.dp"
                ;"test/pattern/bad_pattern1.dp"
                ;"test/pattern/patVatTypeMismatch1.dp"
                ;"test/pattern/patVatTypeMismatch2.dp"
                ;"test/expression/expression1.dp"
                ;"test/expression/expression2.dp"
                ;"test/function/used_func1.dp"
                ;"test/statements/bad_assign1.dp"
                ;"test/statements/bad_assign2.dp"
                ;"test/statements/bad_assign3.dp"
                ;"test/statements/bad_assign4.dp"
                ;"test/statements/bad_break1.dp"
                ;"test/statements/bad_break2.dp"
                ;"test/statements/bad_continue1.dp"
                ;"test/statements/bad_continue2.dp"
                ;"test/statements/bad_return1.dp"
                ;"test/statements/bad_return2.dp"
                ;"test/statements/bad_if.dp"
                ;"test/statements/bad_while.dp"
                ];;

let rec iterator_files fileList = 
    match fileList with
    | []            -> []
    | file::tails   -> (
        Format.printf "%s\n" file;
        List.iter iterator (clauses file);
        Format.printf "\n";
        iterator_files tails
    )

(* let _ = List.iter iterator clauses;; *)
let _ = iterator_files file_list;;

open Semantics.Helper;;
pp_var_table Format.std_formatter table.var;;