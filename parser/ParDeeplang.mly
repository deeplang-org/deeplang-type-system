/* This ocamlyacc file was machine-generated by the BNF converter */
%{
open AbsDeeplang
open Lexing


%}

%token KW_bool KW_tuple KW_i8 KW_i16 KW_i32 KW_i64 KW_u8 KW_u16 KW_u32 KW_u64 KW_f32 KW_f64 KW_char KW_This KW_let KW_mut KW_fun KW_interface KW_extends KW_impl KW_for KW_type KW_as KW_if KW_else KW_while KW_return KW_match KW_true KW_false KW_new

%token SYMB1 /* ; */
%token SYMB2 /* ( */
%token SYMB3 /* ) */
%token SYMB4 /* [ */
%token SYMB5 /* ] */
%token SYMB6 /* -> */
%token SYMB7 /* () */
%token SYMB8 /* , */
%token SYMB9 /* : */
%token SYMB10 /* { */
%token SYMB11 /* } */
%token SYMB12 /* = */
%token SYMB13 /* => */
%token SYMB14 /* _ */
%token SYMB15 /* || */
%token SYMB16 /* && */
%token SYMB17 /* ! */
%token SYMB18 /* < */
%token SYMB19 /* <= */
%token SYMB20 /* > */
%token SYMB21 /* >= */
%token SYMB22 /* == */
%token SYMB23 /* != */
%token SYMB24 /* << */
%token SYMB25 /* >> */
%token SYMB26 /* + */
%token SYMB27 /* - */
%token SYMB28 /* * */
%token SYMB29 /* \ */
%token SYMB30 /* % */
%token SYMB31 /* . */

%token TOK_EOF
%token <string> TOK_Ident
%token <char> TOK_Char
%token <float> TOK_Double
%token <int> TOK_Integer
%token <string> TOK_String
%token <string> TOK_TypeId
%token <string> TOK_VarId

%start pCode_list pCode pTypeT pType1 pType2 pVariable pVariable_list pDeclare pArgs pArg pArg_list pRetType pInterfaceName pInterfaceName_list pMethods pMethodT pMethodT_list pDefine pConstructor pConstructor_list pFields pField pField_list pTypedVar pStatement pIfCondtion pForInit pForCondition pForFinal pMatchBody pMatchCase pMatcher pMatchCase_list pExpression pExpression1 pExpression2 pExpression3 pExpression4 pExpression5 pExpression6 pExpression7 pExpression8 pExpression9 pExpression10 pExpression11 pExpression12 pExpression13 pExpression_list pLiteral pMacro
%type <AbsDeeplang.code list> pCode_list
%type <AbsDeeplang.code> pCode
%type <AbsDeeplang.typeT> pTypeT
%type <AbsDeeplang.typeT> pType1
%type <AbsDeeplang.typeT> pType2
%type <AbsDeeplang.variable> pVariable
%type <AbsDeeplang.variable list> pVariable_list
%type <AbsDeeplang.declare> pDeclare
%type <AbsDeeplang.args> pArgs
%type <AbsDeeplang.arg> pArg
%type <AbsDeeplang.arg list> pArg_list
%type <AbsDeeplang.retType> pRetType
%type <AbsDeeplang.interfaceName> pInterfaceName
%type <AbsDeeplang.interfaceName list> pInterfaceName_list
%type <AbsDeeplang.methods> pMethods
%type <AbsDeeplang.methodT> pMethodT
%type <AbsDeeplang.methodT list> pMethodT_list
%type <AbsDeeplang.define> pDefine
%type <AbsDeeplang.constructor> pConstructor
%type <AbsDeeplang.constructor list> pConstructor_list
%type <AbsDeeplang.fields> pFields
%type <AbsDeeplang.field> pField
%type <AbsDeeplang.field list> pField_list
%type <AbsDeeplang.typedVar> pTypedVar
%type <AbsDeeplang.statement> pStatement
%type <AbsDeeplang.ifCondtion> pIfCondtion
%type <AbsDeeplang.forInit> pForInit
%type <AbsDeeplang.forCondition> pForCondition
%type <AbsDeeplang.forFinal> pForFinal
%type <AbsDeeplang.matchBody> pMatchBody
%type <AbsDeeplang.matchCase> pMatchCase
%type <AbsDeeplang.matcher> pMatcher
%type <AbsDeeplang.matchCase list> pMatchCase_list
%type <AbsDeeplang.expression> pExpression
%type <AbsDeeplang.expression> pExpression1
%type <AbsDeeplang.expression> pExpression2
%type <AbsDeeplang.expression> pExpression3
%type <AbsDeeplang.expression> pExpression4
%type <AbsDeeplang.expression> pExpression5
%type <AbsDeeplang.expression> pExpression6
%type <AbsDeeplang.expression> pExpression7
%type <AbsDeeplang.expression> pExpression8
%type <AbsDeeplang.expression> pExpression9
%type <AbsDeeplang.expression> pExpression10
%type <AbsDeeplang.expression> pExpression11
%type <AbsDeeplang.expression> pExpression12
%type <AbsDeeplang.expression> pExpression13
%type <AbsDeeplang.expression list> pExpression_list
%type <AbsDeeplang.literal> pLiteral
%type <AbsDeeplang.macro> pMacro


%%
pCode_list : code_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pCode : code TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pTypeT : typeT TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pType1 : type1 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pType2 : type2 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pVariable : variable TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pVariable_list : variable_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pDeclare : declare TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pArgs : args TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pArg : arg TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pArg_list : arg_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pRetType : retType TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pInterfaceName : interfaceName TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pInterfaceName_list : interfaceName_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pMethods : methods TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pMethodT : methodT TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pMethodT_list : methodT_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pDefine : define TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pConstructor : constructor TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pConstructor_list : constructor_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pFields : fields TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pField : field TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pField_list : field_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pTypedVar : typedVar TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pStatement : statement TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pIfCondtion : ifCondtion TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pForInit : forInit TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pForCondition : forCondition TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pForFinal : forFinal TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pMatchBody : matchBody TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pMatchCase : matchCase TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pMatcher : matcher TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pMatchCase_list : matchCase_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression : expression TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression1 : expression1 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression2 : expression2 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression3 : expression3 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression4 : expression4 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression5 : expression5 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression6 : expression6 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression7 : expression7 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression8 : expression8 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression9 : expression9 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression10 : expression10 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression11 : expression11 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression12 : expression12 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression13 : expression13 TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pExpression_list : expression_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pLiteral : literal TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };

pMacro : macro TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };


code_list : code SYMB1 code_list { (fun (x,xs) -> x::xs) ($1, $3) }
  | /* empty */ { []  }
;

code : declare { Declares $1 }
  | define { Defines $1 }
  | statement { Statements $1 }
  | expression { Expressions $1 }
  | /* empty */ { Unit  }
;

typeT : type1 {  $1 }
  | SYMB4 typeT SYMB1 int SYMB5 { TypeArray ($2, $4) }
  | SYMB4 typeT SYMB5 { TypeList $2 }
  | typeT SYMB6 typeT { TypeArrow ($1, $3) }
;

type1 : type2 {  $1 }
  | KW_bool { TypeBool  }
  | KW_tuple { TypeTuple  }
  | SYMB7 { TypeUnit  }
  | KW_i8 { TypeI8  }
  | KW_i16 { TypeI16  }
  | KW_i32 { TypeI32  }
  | KW_i64 { TypeI64  }
  | KW_u8 { TypeU8  }
  | KW_u16 { TypeU16  }
  | KW_u32 { TypeU32  }
  | KW_u64 { TypeU64  }
  | KW_f32 { TypeF32  }
  | KW_f64 { TypeF64  }
  | KW_char { TypeChar  }
  | KW_This { TypeThis  }
;

type2 : SYMB2 typeT SYMB3 {  $2 }
  | typeId { TypeX $1 }
;

variable : varId { Variables $1 }
;

variable_list : variable { (fun x -> [x]) $1 }
  | variable SYMB8 variable_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

declare : KW_let varId SYMB9 typeT { DecImmut ($2, $4) }
  | KW_let KW_mut varId SYMB9 typeT { DecMut ($3, $5) }
  | KW_fun varId args retType { DecFunc ($2, $3, $4) }
  | KW_interface interfaceName SYMB10 methods SYMB11 { InterfaceNoExt ($2, $4) }
  | KW_interface interfaceName KW_extends interfaceName_list SYMB10 methods SYMB11 { InterfaceExt ($2, $4, $6) }
;

args : SYMB2 SYMB3 { ArgUnit  }
  | SYMB2 arg_list SYMB3 { ArgExist $2 }
;

arg : varId SYMB9 typeT { ArgCons ($1, $3) }
  | KW_as varId SYMB9 typeT { Delegate ($2, $4) }
;

arg_list : arg { (fun x -> [x]) $1 }
  | arg SYMB8 arg_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

retType : /* empty */ { RetUnit  }
  | SYMB6 typeT { RetExist $2 }
;

interfaceName : typeId { InterfaceNames $1 }
;

interfaceName_list : interfaceName { (fun x -> [x]) $1 }
  | interfaceName SYMB8 interfaceName_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

methods : SYMB10 SYMB11 { InterfaceMethodUnit  }
  | SYMB10 methodT_list SYMB11 { InterfaceMethodExist $2 }
;

methodT : KW_fun varId args retType SYMB1 { InterfaceMethod ($2, $3, $4) }
  | KW_fun varId args retType SYMB10 code_list SYMB11 { ADTMethod ($2, $3, $4, $6) }
;

methodT_list : methodT { (fun x -> [x]) $1 }
  | methodT methodT_list { (fun (x,xs) -> x::xs) ($1, $2) }
;

define : KW_impl interfaceName KW_for typeT SYMB10 methodT_list SYMB11 { InterfaceImpl ($2, $4, $6) }
  | KW_impl typeT SYMB10 methodT_list SYMB11 { RawImpl ($2, $4) }
  | KW_fun varId args retType SYMB10 code_list SYMB11 { DefFunc ($2, $3, $4, $6) }
  | KW_type typeId SYMB4 constructor_list SYMB5 methods { ADT ($2, $4, $6) }
  | KW_let typedVar SYMB12 expression { DefVar ($2, $4) }
  | KW_type typeId args { DefType ($2, $3) }
;

constructor : typeId fields { Constructors ($1, $2) }
;

constructor_list : constructor { (fun x -> [x]) $1 }
  | constructor SYMB8 constructor_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

fields : SYMB2 SYMB3 { FieldUnit  }
  | SYMB2 field_list SYMB3 { FieldExist $2 }
;

field : varId SYMB9 typeT { FieldCons ($1, $3) }
;

field_list : field { (fun x -> [x]) $1 }
  | field SYMB8 field_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

typedVar : varId SYMB9 typeT { ImmutVar ($1, $3) }
  | KW_mut varId SYMB9 typeT { MutVar ($2, $4) }
;

statement : KW_if SYMB2 ifCondtion SYMB3 SYMB10 code_list SYMB11 { If ($3, $6) }
  | KW_if SYMB2 ifCondtion SYMB3 SYMB10 code_list SYMB11 KW_else SYMB10 code_list SYMB11 { IfElse ($3, $6, $10) }
  | KW_for SYMB2 KW_let typedVar SYMB12 expression SYMB1 forCondition SYMB1 forFinal SYMB3 SYMB10 code_list SYMB11 { For ($4, $6, $8, $10, $13) }
  | KW_while SYMB2 forCondition SYMB3 SYMB10 code_list SYMB11 { While ($3, $6) }
  | KW_return expression { Return $2 }
  | KW_match SYMB2 varId SYMB3 SYMB10 matchBody SYMB11 { Match ($3, $6) }
;

ifCondtion : expression { IfCondtions $1 }
;

forInit : /* empty */ { ForInitUnit  }
;

forCondition : expression { ForConditions $1 }
;

forFinal : expression { ForFinals $1 }
;

matchBody : matchCase_list { MatchBodys $1 }
;

matchCase : matcher SYMB13 SYMB10 code_list SYMB11 { MatchCases ($1, $4) }
;

matcher : SYMB14 { WildCardMatch  }
  | constructor { ConsMatch $1 }
  | typedVar { VarMatch $1 }
  | matcher KW_as typedVar { AsVarMatch ($1, $3) }
;

matchCase_list : /* empty */ { []  }
  | matchCase matchCase_list { (fun (x,xs) -> x::xs) ($1, $2) }
;

expression : expression1 {  $1 }
;

expression1 : expression2 {  $1 }
;

expression2 : expression3 {  $1 }
  | expression2 SYMB15 expression3 { ExpLogicalOr ($1, $3) }
;

expression3 : expression4 {  $1 }
  | expression3 SYMB16 expression4 { ExpLogicalAnd ($1, $3) }
;

expression4 : expression5 {  $1 }
  | SYMB17 expression4 { ExpLogicalNot $2 }
;

expression5 : expression6 {  $1 }
  | expression7 SYMB18 expression7 { ExpLt ($1, $3) }
  | expression7 SYMB19 expression7 { ExpLeq ($1, $3) }
  | expression7 SYMB20 expression7 { ExpGt ($1, $3) }
  | expression7 SYMB21 expression7 { ExpGeq ($1, $3) }
;

expression6 : expression7 {  $1 }
  | expression7 SYMB22 expression7 { ExpEq ($1, $3) }
  | expression7 SYMB23 expression7 { ExpNoteq ($1, $3) }
;

expression7 : expression8 {  $1 }
  | expression7 SYMB24 expression8 { ExpLeftShift ($1, $3) }
  | expression7 SYMB25 expression8 { ExpRightShift ($1, $3) }
;

expression8 : expression9 {  $1 }
  | expression8 SYMB26 expression9 { ExpAdd ($1, $3) }
  | expression8 SYMB27 expression9 { ExpSub ($1, $3) }
;

expression9 : expression10 {  $1 }
  | expression9 SYMB28 expression10 { ExpMul ($1, $3) }
  | expression9 SYMB29 expression10 { ExpDiv ($1, $3) }
  | expression9 SYMB30 expression10 { ExpMod ($1, $3) }
;

expression10 : expression11 {  $1 }
;

expression11 : expression12 {  $1 }
  | expression11 SYMB2 expression_list SYMB3 { ExpApp ($1, $3) }
  | KW_new typeId SYMB2 expression_list SYMB3 { ExpNewObj ($2, $4) }
  | expression11 SYMB31 variable { ExpMethod ($1, $3) }
;

expression12 : expression13 {  $1 }
  | variable { ExpVar $1 }
  | literal { Literals $1 }
;

expression13 : SYMB2 expression SYMB3 {  $2 }
  | SYMB2 expression SYMB3 { ExpBracket $2 }
;

expression_list : /* empty */ { []  }
  | expression { (fun x -> [x]) $1 }
  | expression SYMB8 expression_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

literal : string { String $1 }
  | int { Integer $1 }
  | KW_true { True  }
  | KW_false { False  }
;

macro : KW_match SYMB2 code_list SYMB3 { ArrayMatch $3 }
;


int :  TOK_Integer  { $1 };
string : TOK_String { $1 };
typeId : TOK_TypeId { TypeId ($1)};
varId : TOK_VarId { VarId ($1)};


