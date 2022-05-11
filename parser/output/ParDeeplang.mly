/* This ocamlyacc file was machine-generated by the BNF converter */
%{
open AbsDeeplang
open Lexing


%}

%token KW_return KW_true KW_false

%token SYMB8 /* , */
%token SYMB9 /* : */
%token SYMB10 /* {} */
%token SYMB11 /* { */
%token SYMB12 /* } */
%token SYMB13 /* = */
%token SYMB14 /* => */
%token SYMB15 /* _ */
%token SYMB16 /* [] */
%token SYMB17 /* += */
%token SYMB18 /* -= */
%token SYMB19 /* *= */
%token SYMB20 /* /= */
%token SYMB21 /* %= */
%token SYMB22 /* || */
%token SYMB23 /* && */
%token SYMB24 /* ! */
%token SYMB25 /* < */
%token SYMB26 /* <= */
%token SYMB27 /* > */
%token SYMB28 /* >= */
%token SYMB29 /* == */
%token SYMB30 /* != */
%token SYMB31 /* << */
%token SYMB32 /* >> */
%token SYMB33 /* + */
%token SYMB34 /* - */
%token SYMB35 /* * */
%token SYMB36 /* / */
%token SYMB38 /* . */

%token TOK_EOF
%token <string> TOK_Ident
%token <char> TOK_Char
%token <float> TOK_Double
%token <int> TOK_Integer
%token <string> TOK_String
%token <(int * int) * string> TOK_IF
%token <(int * int) * string> TOK_ELSE
%token <(int * int) * string> TOK_WHILE
%token <(int * int) * string> TOK_FOR
%token <(int * int) * string> TOK_IN
%token <(int * int) * string> TOK_LET
%token <(int * int) * string> TOK_FUN
%token <(int * int) * string> TOK_MUT
%token <(int * int) * string> TOK_INTERFACE
%token <(int * int) * string> TOK_IMPL
%token <(int * int) * string> TOK_AS
%token <(int * int) * string> TOK_MATCH
%token <(int * int) * string> TOK_TYPE
%token <(int * int) * string> TOK_EXTENDS
%token <(int * int) * string> TOK_TypeId
%token <(int * int) * string> TOK_BaseType
%token <(int * int) * string> TOK_VarId
%token <(int * int) * string> TOK_SColon
%token <(int * int) * string> TOK_Arrow
%token <(int * int) * string> TOK_LBrack
%token <(int * int) * string> TOK_RBrack
%token <(int * int) * string> TOK_LParen
%token <(int * int) * string> TOK_RParen
%token <(int * int) * string> TOK_Unit
%token <(int * int) * string> TOK_Mod

%start pCode_list
%type <AbsDeeplang.code list> pCode_list


%%
pCode_list : code_list TOK_EOF { $1 }
  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };


code_list : code code_list { (fun (x,xs) -> x::xs) ($1, $2) }
  | code { (fun x -> [x]) $1 }
  | /* empty */ { [] }
;

code : declare { Declares $1 }
  | define { Defines $1 }
  | statement { Statements $1 }
  | expression sCOLON { Expressions $1 }
  | /* empty */ { Unit  }
;

typeT : lBRACK typeT sCOLON int rBRACK { {span = (Parsing.symbol_start_pos (), Parsing.symbol_start_pos ()); typeTShape = TypeFixLenArray ($2, $4)} }
  | typeT aRROW typeT { {span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()); typeTShape = TypeArrow ($1, $3)} }
  | uNIT { {span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()); typeTShape = TypeUnit} }
  | lPAREN rPAREN { {span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()); typeTShape = TypeUnit} }
  | lPAREN typeT_list rPAREN { {span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()); typeTShape = TypeTuple $2} }
  | baseType { {span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()); typeTShape = TypePrimitive $1} }
  | typeId { {span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; typeTShape = TypeX $1} }
;

typeT_list : typeT { (fun x -> [x]) $1 }
  | typeT SYMB8 typeT_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

varId_list : varId { (fun x -> [x]) $1 }
  | varId SYMB8 varId_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

mVarId : mUT varId { {span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; mVarIdShape = (true, $2)} }
  | varId { {span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; mVarIdShape = (false, $1)} }
;

declare : fUN varId args retType { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; declareShape = DecFunc ($1, $2, $3, $4) } }
  | iNTERFACE interfaceName methods { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; declareShape = InterfaceNoExt ($1, $2, $3) } }
  | iNTERFACE interfaceName eXTENDS interfaceName_list methods { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; declareShape = InterfaceExt ($1, $2, $4, $5) } }
;

args : lPAREN rPAREN { ArgUnit }
  | uNIT { ArgUnit }
  | lPAREN arg_list rPAREN { ArgExist $2 }
;

arg : varId SYMB9 typeT { ArgCons ($1, $3) }
;

arg_list : arg { (fun x -> [x]) $1 }
  | arg SYMB8 arg_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

retType : /* empty */ { RetUnit  }
  | aRROW typeT { RetExist $2 }
;

interfaceName : typeId { InterfaceNames $1 }
;

interfaceName_list : interfaceName { (fun x -> [x]) $1 }
  | interfaceName SYMB8 interfaceName_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

methods : SYMB10 { [] }
  | SYMB11 methodT_list SYMB12 { $2 }
;

methodT : fUN varId args retType sCOLON { InterfaceMethod ($1, $2, $3, $4) }
;

methodT_list : /* empty */ { []  }
  | methodT { (fun x -> [x]) $1 }
  | methodT methodT_list { (fun (x,xs) -> x::xs) ($1, $2) }
;

define : functionT { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; defineShape = DefFunc $1 } }
  | tYPE typeId lBRACK constructor_list rBRACK { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; defineShape = ADT ($1, $2, $4) } }
  | tYPE typeId SYMB11 structField_list SYMB12 { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; defineShape = Struct ($1, $2, $4) } }
  | lET mutFlag typedMatcher rHS sCOLON { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; defineShape = DefVar ($1, $2, $3, $4) } }
  | tYPE typeId args sCOLON { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; defineShape = DefType ($1, $2, $3) } }
  | iMPL interfaceName fOR typeT functions { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; defineShape = InterfaceImpl ($1, $2, $3, $4, $5) } }
  | iMPL typeT functions { { span = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) ; defineShape = RawImpl ($1, $2, $3) } }
;

functionT : fUN varId args retType SYMB10 { Func ($1, $2, $3, $4, []) }
  | fUN varId args retType SYMB11 statement_list SYMB12 { Func ($1, $2, $3, $4, $6) }
;

constructor : typeId { UnitCons $1 }
  | typeId lPAREN field_list rPAREN { ParamCons ($1, $3) }
;

constructor_list : constructor { (fun x -> [x]) $1 }
  | constructor SYMB8 constructor_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

field : varId SYMB9 typeT { FieldCons ($1, $3) }
;

field_list : field { (fun x -> [x]) $1 }
  | field SYMB8 field_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

structField : field { BasicStructField $1 }
  | aS field { DelegateStructField ($1, $2) }
;

structField_list : structField { (fun x -> [x]) $1 }
  | structField SYMB8 structField_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

rHS : SYMB13 expression { DefRHS $2 }
  | /* empty */ { NilRHS  }
;

mutFlag : mUT { Mut $1 }
  | /* empty */ { Immut  }
;

functions : SYMB10 { FunctionsUnit  }
  | SYMB11 functionT_list SYMB12 { FunctionsMany $2 }
;

functionT_list : /* empty */ { []  }
  | functionT { (fun x -> [x]) $1 }
  | functionT functionT_list { (fun (x,xs) -> x::xs) ($1, $2) }
;

statement : SYMB11 statement_list SYMB12 { Block $2 }
  | lET mutFlag typedMatcher rHS sCOLON { DefVarSt ($1, $2, $3, $4) }
  | expression sCOLON { ExprSt $1 }
  | KW_return expression sCOLON { Return $2 }
  | iF lPAREN expression rPAREN SYMB11 statement_list SYMB12 elseBody { If ($1, $3, $6, $8) }
  | fOR lPAREN matcher iN expression rPAREN SYMB11 statement_list SYMB12 { For ($1, $3, $4, $5, $8) }
  | wHILE lPAREN expression rPAREN SYMB11 statement_list SYMB12 { While ($1, $3, $6) }
  | mATCH lPAREN varId rPAREN SYMB11 matchBody SYMB12 { Match ($1, $3, $6) }
;

statement_list : statement { (fun x -> [x]) $1 }
  | statement statement_list { (fun (x,xs) -> x::xs) ($1, $2) }
  | /* empty */ { [] }
;

elseBody : /* empty */ { NoElse  }
  | eLSE iF lPAREN expression rPAREN SYMB11 statement_list SYMB12 elseBody { Elif ($1, $2, $4, $7, $9) }
  | eLSE SYMB11 statement_list SYMB12 { Else ($1, $3) }
;

matchBody : matchCase_list { MatchBodys $1 }
;

matchCase : matcher SYMB14 SYMB11 statement_list SYMB12 { MatchCases ($1, $4) }
;

matcher : typedMatcher { TypedMatchers $1 }
  | typelessMatcher { TypelessMatchers $1 }
  | matcher aS mVarId { AsVarMatch ($1, $2, $3) }
;

typedMatcher : typelessMatcher SYMB9 typeT { Typed ($1, $3) }
;

typelessMatcher : SYMB15 { WildCardMatch  }
  | typeId uNIT { ConsMatchUnit $1 }
  | typeId lPAREN matcher rPAREN { ConsMatch ($1, $3) }
  | mVarId { TypelessVarMatch $1 }
  | uNIT { UnitMatch }
  | lPAREN matcher_list rPAREN { TupleMatch $2 }
  | literal { LiteralMatch $1 }
  | typeId SYMB10 { FieldMatchUnit $1 }
  | typeId SYMB11 fieldMatcher_list SYMB12 { FieldMatch ($1, $3) }
;

matcher_list : /* empty */ { []  }
  | matcher { (fun x -> [x]) $1 }
  | matcher SYMB8 matcher_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

fieldMatcher_list : /* empty */ { []  }
  | fieldMatcher { (fun x -> [x]) $1 }
  | fieldMatcher SYMB8 fieldMatcher_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

fieldMatcher : varId SYMB9 typelessMatcher { FieldMatchers ($1, $3) }
;

matchCase_list : matchCase { (fun x -> [x]) $1 }
  | matchCase matchCase_list { (fun (x,xs) -> x::xs) ($1, $2) }
;

expression : expression1 {  $1 }
;

expression1 : expression2 {  $1 }
  | varId SYMB13 expression1 { ExpAssignment ($1, $3) }
  | varId SYMB17 expression1 { ExpAssignmentPlus ($1, $3) }
  | varId SYMB18 expression1 { ExpAssignmentMinus ($1, $3) }
  | varId SYMB19 expression1 { ExpAssignmentMul ($1, $3) }
  | varId SYMB20 expression1 { ExpAssignmentDiv ($1, $3) }
  | varId SYMB21 expression1 { ExpAssignmentMod ($1, $3) }
;

expression2 : expression3 {  $1 }
  | expression2 SYMB22 expression3 { ExpLogicalOr ($1, $3) }
;

expression3 : expression4 {  $1 }
  | expression3 SYMB23 expression4 { ExpLogicalAnd ($1, $3) }
;

expression4 : expression5 {  $1 }
  | SYMB24 expression4 { ExpLogicalNot $2 }
;

expression5 : expression6 {  $1 }
  | expression7 SYMB25 expression7 { ExpLt ($1, $3) }
  | expression7 SYMB26 expression7 { ExpLeq ($1, $3) }
  | expression7 SYMB27 expression7 { ExpGt ($1, $3) }
  | expression7 SYMB28 expression7 { ExpGeq ($1, $3) }
;

expression6 : expression7 {  $1 }
  | expression7 SYMB29 expression7 { ExpEq ($1, $3) }
  | expression7 SYMB30 expression7 { ExpNoteq ($1, $3) }
;

expression7 : expression8 {  $1 }
  | expression7 SYMB31 expression8 { ExpLeftShift ($1, $3) }
  | expression7 SYMB32 expression8 { ExpRightShift ($1, $3) }
;

expression8 : expression9 {  $1 }
  | expression8 SYMB33 expression9 { ExpAdd ($1, $3) }
  | expression8 SYMB34 expression9 { ExpSub ($1, $3) }
;

expression9 : expression10 {  $1 }
  | expression9 SYMB35 expression10 { ExpMul ($1, $3) }
  | expression9 SYMB36 expression10 { ExpDiv ($1, $3) }
  | expression9 mOD expression10 { ExpMod ($1, $3) }
;

expression10 : expression11 {  $1 }
;

expression11 : expression12 { $1 }
  | expression11 lPAREN expression_list rPAREN { ExpApp ($1, $3) }
  | expression11 uNIT { ExpAppUnit $1 }
  | typeId lPAREN expression_list rPAREN { ExpNewObj ($1, $3) }
  | typeId uNIT { ExpNewObjUnit $1 }
  | expression11 SYMB38 varId { ExpMethod ($1, $3) }
;

expression12 : expression13 {  $1 }
  | matcher { ExpVar $1 }
  | literal { Literals $1 }
  | lPAREN expression_list rPAREN { Tuples $2 }
  | lBRACK expression_list rBRACK { Array $2 }
  | typeId SYMB11 fieldInit_list SYMB12 { StructInit ($1, $3) }
;

expression13 : lPAREN expression rPAREN {  $2 }
  | lPAREN expression rPAREN { ExpBracket $2 }
;

expression_list : /* empty */ { [] }
  | expression { (fun x -> [x]) $1 }
  | expression SYMB8 expression_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

literal : string { String $1 }
  | char { Char $1 }
  | int { Integer $1 }
  | float { Float $1 }
  | KW_true { True }
  | KW_false { False }
  | uNIT { LUnit }
  | SYMB16 { AUnit }
;

fieldInit_list : fieldInit { (fun x -> [x]) $1 }
  | fieldInit SYMB8 fieldInit_list { (fun (x,xs) -> x::xs) ($1, $3) }
;

fieldInit : varId SYMB9 expression { FieldInitCons ($1, $3) }
;


char : TOK_Char { $1 };
float : TOK_Double  { $1 };
int :  TOK_Integer  { $1 };
string : TOK_String { $1 };
iF : TOK_IF { IF ($1)};
eLSE : TOK_ELSE { ELSE ($1)};
wHILE : TOK_WHILE { WHILE ($1)};
fOR : TOK_FOR { FOR ($1)};
iN : TOK_IN { IN ($1)};
lET : TOK_LET { LET ($1)};
fUN : TOK_FUN { FUN ($1)};
mUT : TOK_MUT { MUT ($1)};
iNTERFACE : TOK_INTERFACE { INTERFACE ($1)};
iMPL : TOK_IMPL { IMPL ($1)};
aS : TOK_AS { AS ($1)};
mATCH : TOK_MATCH { MATCH ($1)};
tYPE : TOK_TYPE { TYPE ($1)};
eXTENDS : TOK_EXTENDS { EXTENDS ($1)};
typeId : TOK_TypeId { TypeId ($1)};
baseType : TOK_BaseType { BaseType ($1)};
varId : TOK_VarId { VarId ($1)};
sCOLON : TOK_SColon { SYMBOL ($1)};
aRROW : TOK_Arrow { SYMBOL ($1)};
lBRACK : TOK_LBrack { SYMBOL ($1) };
rBRACK : TOK_RBrack { SYMBOL ($1) };
lPAREN : TOK_LParen { SYMBOL ($1) };
rPAREN : TOK_RParen { SYMBOL ($1) };
uNIT : TOK_Unit { SYMBOL ($1) };
mOD : TOK_Mod { SYMBOL ($1) };
