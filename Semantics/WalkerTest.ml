
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

(* open Semantics.SemanticsError;; *)

let clauses file = try parse_file file with
    Syntax.SyntaxError.Error(span, err) ->
        Format.printf "syntax error: %a@ in %a"
            Syntax.SyntaxError.pp_error err Syntax.SyntaxError.pp_span span;
        exit 1
    ;;

let iterator clause = 
    try walk clause with
        Semantics.SemanticsError.ErrorType(err) ->
            Format.printf "semantics error: %a\n"
            Semantics.SemanticsError.print_error err;;
    (* @todo 未来加入定位报错时这里也需要修改 *)
        (* Format.printf "semantics error: %a@ in %a"
        Semantics.SemanticsError.print_error err 
            Syntax.SyntaxError.pp_span span; *)

let file_list = ["test/type.dp"
                ;"test/type/unsupport_type1.dp"
                (* ;"test/type/unsupport_type2.dp" *)
                ;"test/expression/expression1.dp"
                ;"test/expression/expression2.dp"
                ;"test/pattern/bad_pattern1.dp"
                ;"test/used_func1.dp"
                ;"test/used_var1.dp"
                ;"test/used_var2.dp"
                ;"test/used_var3.dp"
                ;"test/used_var4.dp"
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