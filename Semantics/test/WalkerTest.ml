
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

let file_list = ["type.dp"
                ;"type/unsupport_type1.dp"
                ;"type/unsupport_type2.dp"
                ;"type/used_var1.dp"
                ;"type/used_var2.dp"
                ;"type/used_var3.dp"
                ;"type/used_var4.dp"
                (* ;"type/unsupport_type2.dp" *)
                ;"pattern/bad_pattern1.dp"
                ;"pattern/patVatTypeMismatch1.dp"
                ;"pattern/patVatTypeMismatch2.dp"
                ;"expression/expression1.dp"
                ;"expression/expression2.dp"
                ;"function/used_func1.dp"
                ;"expression/negativeNaN.dp"
                ;"expression/notNaB.dp"
                ;"expression/errorOrdering.dp"
                ;"expression/errorEqualing.dp"
                ;"expression/errorUnequaling.dp"
                ;"expression/errorOrderingType.dp"
                ;"expression/errorLogicalOpType.dp"
                ;"expression/errorLogicalOpType2.dp"
                ;"expression/shiftNonInt.dp"
                ;"expression/shiftNaNbits.dp"
                ;"expression/arithmeticOnErrorType.dp"
                ;"expression/modOnErrorType.dp"
                ;"expression/modOnDifferentTypes.dp"
                ;"expression/ADThasNoFields.dp"
                ;"expression/structTypeNotFound.dp"
                (* ;"expression/errorUsingADT.dp" *)
                (* ;"expression/errorIfReturn.dp" *)
                ;"expression/errorIfCondition.dp"
                ;"expression/errorMatchStruct.dp"
                (* ;"expression/errorInterface.dp" *)
                ;"statements/bad_assign1.dp"
                ;"statements/bad_assign2.dp"
                ;"statements/bad_assign3.dp"
                ;"statements/bad_assign4.dp"
                ;"statements/bad_break1.dp"
                ;"statements/bad_break2.dp"
                ;"statements/bad_continue1.dp"
                ;"statements/bad_continue2.dp"
                ;"statements/bad_return1.dp"
                ;"statements/bad_return2.dp"
                ;"statements/bad_if.dp"
                ;"statements/bad_while.dp"
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
