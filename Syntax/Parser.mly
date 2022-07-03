/* This ocamlyacc file was machine-generated by the BNF converter */
%{
open ParseTree
open SyntaxError


let cur_span () =
    { span_start = Parsing.symbol_start_pos ()
    ; span_end   = Parsing.symbol_end_pos () }

let error err = raise(Error(cur_span (), err))


let make_id_generator () =
    let seed = ref (-1) in
    fun () -> incr seed; !seed

let new_symbol  = make_id_generator ()
let new_pat_id  = make_id_generator ()
let new_expr_id = make_id_generator ()
let new_stmt_id = make_id_generator ()
let new_impl_id = make_id_generator ()

let mk_typ  shape : typ = { shape; span = cur_span () }
let mk_pat  shape = { shape; span = cur_span (); pat_id  = NodeId.PatId  (new_pat_id  ()) }
let mk_expr shape = { shape; span = cur_span (); expr_id = NodeId.ExprId (new_expr_id ()) }
let mk_stmt shape = { shape; span = cur_span (); stmt_id = NodeId.StmtId (new_stmt_id ()) }

let mk_var_pat mut typ name =
    { vpat_mut  = mut
    ; vpat_typ  = typ
    ; vpat_name = name
    ; vpat_symb = Symbol (new_symbol ()) }

let mk_func_arg name typ =
    { farg_name = name
    ; farg_symb = Symbol (new_symbol ())
    ; farg_typ  = typ }

let mk_impl intf typ methods =
    { impl_intf    = intf
    ; impl_typ     = typ
    ; impl_id      = NodeId.ImplId (new_impl_id ())
    ; impl_methods = methods }

let mk_global_var name value =
    { gvar_name  = name
    ; gvar_id    = Symbol (new_symbol ())
    ; gvar_value = value }

let mk_top_clause shape = { shape; span = cur_span () }
%}


%token TOK_EOF

%token TOK_LPAREN     /* ( */
%token TOK_RPAREN     /* ) */
%token TOK_LBRACK     /* [ */
%token TOK_RBRACK     /* ] */
%token TOK_LBRACE     /* { */
%token TOK_RBRACE     /* } */
%token TOK_COLON      /* : */
%token TOK_COMMA      /* , */
%token TOK_SEMICOLON  /* ; */
%token TOK_EQ         /* = */
%token TOK_EQGT       /* => */
%token TOK_MINUSGT    /* -> */
%token TOK_DOT        /* . */
%token TOK_UNDERSCORE /* _ */

%token TOK_MINUS  /* - */
%token TOK_BANG   /* ! */

%token TOK_LT     /* <  */
%token TOK_LTEQ   /* <= */
%token TOK_GT     /* >  */
%token TOK_GTEQ   /* >= */
%token TOK_EQEQ   /* == */
%token TOK_BANGEQ /* != */
%token TOK_LOR    /* || */
%token TOK_LAND   /* && */
%token TOK_LXOR   /* ^^ */

%token TOK_BOR    /* |  */
%token TOK_BAND   /* &  */
%token TOK_BXOR   /* ^  */
%token TOK_LSHIFT /* << */
%token TOK_RSHIFT /* >> */
%token TOK_ADD    /* +  */
/* TOK_MINUS */
%token TOK_MUL    /* *  */
%token TOK_DIV    /* /  */
%token TOK_MOD    /* %  */

%token TOK_BOREQ    /* |=  */
%token TOK_BANDEQ   /* &=  */
%token TOK_BXOREQ   /* ^=  */
%token TOK_LSHIFTEQ /* <<= */
%token TOK_RSHIFTEQ /* >>= */
%token TOK_ADDEQ    /* +=  */
%token TOK_MINUSEQ  /* -=  */
%token TOK_MULEQ    /* *=  */
%token TOK_DIVEQ    /* /=  */
%token TOK_MODEQ    /* %=  */

%token <string> TOK_UpperIdent
%token <string> TOK_LowerIdent
%token <int>    TOK_Integer
%token <float>  TOK_Double
%token <int>    TOK_Char
%token <string> TOK_String

%token TOK_TyBool
%token TOK_TyChar
%token TOK_TyThis
%token <ParseTree.int_typ_sign * ParseTree.int_typ_size> TOK_TyInt 
%token <ParseTree.float_typ_size> TOK_TyFloat

%token TOK_TRUE TOK_FALSE
%token TOK_THIS
%token TOK_AS
%token TOK_IF TOK_ELSE
%token TOK_WHILE TOK_FOR TOK_BREAK TOK_CONTINUE
%token TOK_LET TOK_MUT TOK_IN
%token TOK_MATCH
%token TOK_FUN TOK_RETURN 
%token TOK_INTERFACE
%token TOK_IMPL TOK_EXTENDS
%token TOK_TYPE


%left     TOK_LOR
%left     TOK_LXOR
%left     TOK_LAND
%left     TOK_BOR
%left     TOK_BXOR
%left     TOK_BAND
%nonassoc TOK_LT TOK_LTEQ TOK_GT TOK_GTEQ TOK_EQEQ TOK_BANGEQ
%left     TOK_LSHIFT TOK_RSHIFT
%left     TOK_ADD TOK_MINUS
%left     TOK_MUL TOK_DIV TOK_MOD


%start program
%type <ParseTree.top_clause list> program


%%

program :
    | TOK_EOF            { [] }
    | top_clause program { $1 :: $2 }
    | error              { error @@ Expecting "top level clause" }
;

top_clause :
    | TOK_TYPE TOK_UpperIdent TOK_LBRACE struct_fields TOK_RBRACE
        { mk_top_clause @@ StructDef
            { struct_name = $2; struct_fields = $4 } }
    | TOK_TYPE TOK_UpperIdent TOK_LBRACK adt_branches TOK_RBRACK
        { mk_top_clause @@ ADTDef
            { adt_name = $2; adt_branches = $4 } }
    | TOK_INTERFACE TOK_UpperIdent TOK_LBRACE function_decls TOK_RBRACE
        { mk_top_clause @@ InterfaceDecl
            { intf_decl_name = $2; intf_decl_methods = $4 } }
    | TOK_INTERFACE TOK_UpperIdent
        TOK_EXTENDS interface_name_list_nonempty
        TOK_LBRACE function_decls TOK_RBRACE
        { failwith "unimplemented" }
    | TOK_IMPL TOK_UpperIdent TOK_LBRACE function_impls TOK_RBRACE
        { mk_top_clause @@ MethodsImpl (mk_impl None $2 $4) }
    | TOK_IMPL TOK_UpperIdent
        TOK_FOR TOK_UpperIdent
        TOK_LBRACE function_impls TOK_RBRACE
        { mk_top_clause @@ MethodsImpl (mk_impl (Some $4) $2 $6) }
    | function_impl
        { mk_top_clause @@ FunctionDef $1 }
    | TOK_LET TOK_LowerIdent TOK_EQ expr TOK_SEMICOLON
        { mk_top_clause @@ GlobalVarDef (mk_global_var $2 $4) }
;


struct_fields :
    | /* empty */                          { [] }
    | struct_field                         { [$1] }
    | struct_field TOK_COMMA struct_fields { $1 :: $3 }
;

struct_field :
    | TOK_LowerIdent TOK_COLON typ        { StructField($1, $3) }
    | TOK_AS TOK_LowerIdent TOK_COLON typ { StructDelegate($2, $4) }
;


adt_branches :
    | /* empty */                       { [] }
    | adt_branch                        { [$1] }
    | adt_branch TOK_COMMA adt_branches { $1 :: $3 }
;

adt_branch :
    | TOK_UpperIdent                                         { ($1, []) }
    | TOK_UpperIdent TOK_LPAREN typ_list_nonempty TOK_RPAREN { ($1, $3) }
;


interface_name_list_nonempty :
    | TOK_UpperIdent                                        { [$1] }
    | TOK_UpperIdent TOK_COMMA interface_name_list_nonempty { $1 :: $3 }
;

function_decls :
    | /* empty */                                { [] }
    | function_decl                              { [$1] }
    | function_decl TOK_SEMICOLON function_decls { $1 :: $3 }
;

function_decl :
    | TOK_FUN TOK_LowerIdent
        TOK_LPAREN function_decl_args TOK_RPAREN
        function_decl_ret
        { { func_decl_name = $2
          ; func_decl_args = $4
          ; func_decl_ret  = $6 } }
;

function_decl_args :
    | /* empty */                 { [] }
    | function_decl_args_nonempty { $1 }
;

function_decl_args_nonempty :
    | function_decl_arg                                       { [$1] }
    | function_decl_arg TOK_COMMA function_decl_args_nonempty { $1 :: $3 }
;

function_decl_arg :
    | TOK_LowerIdent TOK_COLON typ { mk_func_arg $1 $3 }
;

function_decl_ret :
    | TOK_MINUSGT typ { $2 }
    | /* empty */     { mk_typ TyUnit }
;


function_impls :
    | /* empty */                                { [] }
    | function_impl                              { [$1] }
    | function_impl TOK_SEMICOLON function_impls { $1 :: $3 }
;

function_impl :
    | function_decl TOK_LBRACE stmt_list TOK_RBRACE
        { ( $1, mk_stmt @@ StmtSeq $3 ) }
;



typ :
    | TOK_LPAREN TOK_RPAREN { mk_typ TyUnit }
    | TOK_TyBool            { mk_typ TyBool }
    | TOK_TyInt             { mk_typ @@ TyInt(fst $1, snd $1) }
    | TOK_TyFloat           { mk_typ @@ TyFloat $1 }
    | TOK_TyChar            { mk_typ TyChar }
    | TOK_TyThis            { mk_typ TyThis }
    | TOK_UpperIdent        { mk_typ @@ TyNamed($1, []) }
    | TOK_LBRACK typ TOK_SEMICOLON TOK_Integer TOK_RBRACK
        { mk_typ @@ TyArray($2, $4) }
    | TOK_LPAREN typ_list_nonempty TOK_RPAREN
        { mk_typ @@ TyTuple $2 }
    | error
        { error @@ Expecting "type" }
;

typ_list_nonempty :
    | typ                             { [$1] }
    | typ TOK_COMMA typ_list_nonempty { $1 :: $3 }
;




stmt_list :
    | /* empty */    { [] }
    | stmt stmt_list { $1 :: $2 }
;

stmt :
    | expr TOK_SEMICOLON            { mk_stmt @@ StmtExpr $1 }
    | TOK_RETURN expr TOK_SEMICOLON { mk_stmt @@ StmtReturn $2 }
    | TOK_BREAK TOK_SEMICOLON       { mk_stmt @@ StmtBreak }
    | TOK_CONTINUE TOK_SEMICOLON    { mk_stmt @@ StmtContinue }
    | lvalue assignment_op expr TOK_SEMICOLON
        { mk_stmt @@ StmtAssign($2, (mk_expr @@ ExpVar $1), $3) }
    | TOK_LET pattern TOK_EQ expr TOK_SEMICOLON
        { mk_stmt @@ StmtDecl($2, $4) }
    | TOK_LBRACE stmt_list TOK_RBRACE { mk_stmt @@ StmtSeq $2 }
    | TOK_IF TOK_LPAREN expr TOK_RPAREN stmt
        { mk_stmt @@ StmtIf($3, $5, None) }
    | TOK_IF TOK_LPAREN expr TOK_RPAREN stmt TOK_ELSE stmt
        { mk_stmt @@ StmtIf($3, $5, Some $7) }
    | TOK_FOR
        TOK_LPAREN pattern TOK_IN expr TOK_RPAREN
        stmt
        { mk_stmt @@ StmtFor($3, $5, $7) }
    | TOK_WHILE TOK_LPAREN expr TOK_RPAREN stmt
        { mk_stmt @@ StmtWhile($3, $5) }
    | TOK_MATCH TOK_LPAREN expr TOK_RPAREN TOK_LBRACE match_branches TOK_RBRACE
        { mk_stmt @@ StmtMatch($3, $6) }
    | error
        { error @@ Expecting "statement" }
;

lvalue :
    | TOK_LowerIdent { $1 }
;

assignment_op :
    | TOK_EQ       { None             }
    | TOK_BOREQ    { Some BinOpBOr    }
    | TOK_BANDEQ   { Some BinOpBAnd   }
    | TOK_BXOREQ   { Some BinOpBXor   }
    | TOK_LSHIFTEQ { Some BinOpLShift }
    | TOK_RSHIFTEQ { Some BinOpRShift }
    | TOK_ADDEQ    { Some BinOpAdd    }
    | TOK_MINUSEQ  { Some BinOpSub    }
    | TOK_MULEQ    { Some BinOpMul    }
    | TOK_DIVEQ    { Some BinOpDiv    }
    | TOK_MODEQ    { Some BinOpMod    }
;

match_branches :
    | /* empty */                 { [] }
    | match_branch match_branches { $1 :: $2 }
;

match_branch :
    | pattern TOK_EQGT stmt { ($1, $3) }
;
    

literal :
    | TOK_LPAREN TOK_RPAREN { LitUnit }
    | TOK_TRUE              { LitBool true }
    | TOK_FALSE             { LitBool false }
    | TOK_Integer           { LitInt $1 }
    | TOK_Double            { LitFloat $1 }
    | TOK_Char              { LitChar $1 }
    | TOK_String            { LitString $1 }
;


pattern :
    | TOK_UNDERSCORE   { mk_pat PatWildcard }
    | literal          { mk_pat @@ PatLit $1 }
    | variable_pattern { mk_pat @@ PatVar $1 }
    | pattern TOK_AS variable_pattern
        { mk_pat @@ PatAs($1, $3) }
    | TOK_UpperIdent
        { mk_pat @@ PatADT($1, []) }
    | TOK_UpperIdent TOK_LPAREN pattern_list_nonempty TOK_RPAREN
        { mk_pat @@ PatADT($1, $3) }
    | TOK_UpperIdent TOK_LBRACE struct_pattern_fields TOK_RBRACE
        { mk_pat @@ PatStruct($1, $3) }
    | TOK_LPAREN pattern_list_nonempty TOK_RPAREN
        { mk_pat @@ PatTuple $2 }
    | error
        { error @@ Expecting "pattern" }
;

variable_pattern :
    | TOK_LowerIdent                       { mk_var_pat Imm None $1 }
    | TOK_MUT TOK_LowerIdent               { mk_var_pat Mut None $2 }
    | TOK_LowerIdent TOK_COLON typ         { mk_var_pat Imm (Some $3) $1 }
    | TOK_MUT TOK_LowerIdent TOK_COLON typ { mk_var_pat Mut (Some $4) $2 }
;

pattern_list_nonempty :
    | pattern                                 { [$1] }
    | pattern TOK_COMMA pattern_list_nonempty { $1 :: $3 }
;


struct_pattern_fields :
    | /* empty */                                          { [] }
    | struct_pattern_field                                 { [$1] }
    | struct_pattern_field TOK_COMMA struct_pattern_fields { $1 :: $3 }
;

struct_pattern_field :
    | TOK_LowerIdent TOK_COLON pattern { ($1, $3) }
;



expr :
    | small_expr           { $1 }
    | expr TOK_LT     expr { mk_expr @@ ExpBinOp(BinOpCompare   BinOpLt    , $1, $3) }
    | expr TOK_LTEQ   expr { mk_expr @@ ExpBinOp(BinOpCompare   BinOpLeq   , $1, $3) }
    | expr TOK_GT     expr { mk_expr @@ ExpBinOp(BinOpCompare   BinOpGt    , $1, $3) }
    | expr TOK_GTEQ   expr { mk_expr @@ ExpBinOp(BinOpCompare   BinOpGeq   , $1, $3) }
    | expr TOK_EQEQ   expr { mk_expr @@ ExpBinOp(BinOpCompare   BinOpEq    , $1, $3) }
    | expr TOK_BANGEQ expr { mk_expr @@ ExpBinOp(BinOpCompare   BinOpNeq   , $1, $3) }
    | expr TOK_LOR    expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpLOr   , $1, $3) }
    | expr TOK_LAND   expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpLAnd  , $1, $3) }
    | expr TOK_LXOR   expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpLXor  , $1, $3) }
    | expr TOK_BOR    expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpBOr   , $1, $3) }
    | expr TOK_BAND   expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpBAnd  , $1, $3) }
    | expr TOK_BXOR   expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpBXor  , $1, $3) }
    | expr TOK_LSHIFT expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpLShift, $1, $3) }
    | expr TOK_RSHIFT expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpRShift, $1, $3) }
    | expr TOK_ADD    expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpAdd   , $1, $3) }
    | expr TOK_MINUS  expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpSub   , $1, $3) }
    | expr TOK_MUL    expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpMul   , $1, $3) }
    | expr TOK_DIV    expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpDiv   , $1, $3) }
    | expr TOK_MOD    expr { mk_expr @@ ExpBinOp(BinOpCalculate BinOpMod   , $1, $3) }
    | error
        { error @@ Expecting "expression" }
;

small_expr :
    | atom_expr           { $1 }
    | TOK_MINUS atom_expr { mk_expr @@ ExpUnOp(UnOpNeg, $2) }
    | TOK_BANG  atom_expr { mk_expr @@ ExpUnOp(UnOpNot, $2) }
    | TOK_UpperIdent TOK_LBRACE struct_expr_fields TOK_RBRACE
        { mk_expr @@ ExpStruct($1, $3) }
;

atom_expr :
    | literal        { mk_expr @@ ExpLit $1 }
    | TOK_LowerIdent { mk_expr @@ ExpVar $1 }
    | TOK_THIS       { mk_expr ExpThis }
    | TOK_LPAREN expr_list_nonempty TOK_RPAREN
        { match $2 with [expr] -> expr
                      | exprs -> mk_expr @@ ExpTuple exprs }
    | TOK_UpperIdent
        { mk_expr @@ ExpADT($1, []) }
    | TOK_UpperIdent TOK_LPAREN expr_list_nonempty TOK_RPAREN
        { mk_expr @@ ExpADT($1, $3) }
    | TOK_LowerIdent TOK_LPAREN expr_list TOK_RPAREN
        { mk_expr @@ ExpApp($1, $3) }
    | atom_expr TOK_DOT TOK_LowerIdent
        { mk_expr @@ ExpField($1, $3) }
    | atom_expr TOK_DOT TOK_LowerIdent TOK_LPAREN expr_list TOK_RPAREN
        { mk_expr @@ ExpMethod($1, $3, $5) }
;

struct_expr_fields :
    | /* empty */                                    { [] }
    | struct_expr_field                              { [$1] }
    | struct_expr_field TOK_COMMA struct_expr_fields { $1 :: $3 }
;

struct_expr_field :
    | TOK_LowerIdent TOK_COLON expr { ($1, $3) }
;


expr_list_nonempty :
    | expr                              { [$1] }
    | expr TOK_COMMA expr_list_nonempty { $1 :: $3 }
;

expr_list :
    | /* empty */        { [] }
    | expr_list_nonempty { $1 }
;
