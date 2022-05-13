(* OCaml module generated by the BNF converter *)

open Lexing

type iF = IF of ((int * int) * string)
and eLSE = ELSE of ((int * int) * string)
and wHILE = WHILE of ((int * int) * string)
and fOR = FOR of ((int * int) * string)
and iN = IN of ((int * int) * string)
and lET = LET of ((int * int) * string)
and fUN = FUN of ((int * int) * string)
and mUT = MUT of ((int * int) * string)
and iNTERFACE = INTERFACE of ((int * int) * string)
and iMPL = IMPL of ((int * int) * string)
and aS = AS of ((int * int) * string)
and mATCH = MATCH of ((int * int) * string)
and tYPE = TYPE of ((int * int) * string)
and eXTENDS = EXTENDS of ((int * int) * string)
and typeId = TypeId of ((int * int) * string)
and baseType = BaseType of ((int * int) * string)
and varId = VarId of ((int * int) * string)
and symbol = SYMBOL of ((int * int) * string)
and code =
   Declares of declare
 | Defines of define
 | Statements of statement
 | Expressions of expression
 | Unit

and typeT = { span : (position * position) ; typeTShape : typeTShape }
and typeTShape =
   TypeFixLenArray of typeT * int
 | TypeArrow of typeT * typeT
 | TypeUnit
 | TypeTuple of typeT list
 | TypePrimitive of baseType
 | TypeX of typeId

and mVarId = { span : (position * position) ; mVarIdShape : bool * varId }

and declare = { span : (position * position) ; declareShape : declareShape }
and declareShape =
   DecFunc of fUN * varId * args * retType
 | InterfaceNoExt of iNTERFACE * interfaceName * methodT list
 | InterfaceExt of iNTERFACE * interfaceName * interfaceName list * methodT list

and args =
   ArgUnit
 | ArgExist of arg list

and arg =
   ArgCons of varId * typeT

and retType =
   RetUnit
 | RetExist of typeT

and interfaceName =
   InterfaceNames of typeId

and methodT =
   InterfaceMethod of fUN * varId * args * retType

and define = { span : (position * position) ; defineShape : defineShape }
and defineShape =
   DefFunc of functionT
 | ADT of tYPE * typeId * constructor list
 | Struct of tYPE * typeId * structField list
 | DefVar of lET * bool * typedMatcher * rHS
 | DefType of tYPE * typeId * args
 | InterfaceImpl of iMPL * interfaceName * fOR * typeT * functionT list
 | RawImpl of iMPL * typeT * functionT list

and functionT =
   Func of fUN * varId * args * retType * statement list

and constructor = { span : (position * position) ; constructorShape : typeId * field list }

and field = { span : (position * position) ; fieldShape : varId * typeT }

and structField = { span : (position * position) ; structFieldShape : bool * field }

and rHS =
   DefRHS of expression
 | NilRHS

and statement = { span : (position * position) ; statementShape : statementShape }
and statementShape =
   Block of statement list
 | DefVarSt of lET * bool * typedMatcher * rHS
 | ExprSt of expression
 | Return of expression
 | If of iF * expression * statement list * statement list
 | For of fOR * matcher * iN * expression * statement list
 | While of wHILE * expression * statement list
 | Match of mATCH * varId * matchBody

and matchBody =
   MatchBodys of matchCase list

and matchCase =
   MatchCases of matcher * statement list

and matcher =
   TypedMatchers of typedMatcher
 | TypelessMatchers of typelessMatcher
 | AsVarMatch of matcher * aS * mVarId

and typedMatcher =
   Typed of typelessMatcher * typeT

and typelessMatcher =
   WildCardMatch
 | ConsMatchUnit of typeId
 | ConsMatch of typeId * matcher
 | TypelessVarMatch of mVarId
 | UnitMatch
 | TupleMatch of matcher list
 | LiteralMatch of literal
 | FieldMatchUnit of typeId
 | FieldMatch of typeId * fieldMatcher list

and fieldMatcher =
   FieldMatchers of varId * typelessMatcher

and expression =
   ExpVar of matcher
 | Literals of literal
 | Tuples of expression list
 | Array of expression list
 | StructInit of typeId * fieldInit list
 | ExpAssignment of varId * expression
 | ExpAssignmentPlus of varId * expression
 | ExpAssignmentMinus of varId * expression
 | ExpAssignmentMul of varId * expression
 | ExpAssignmentDiv of varId * expression
 | ExpAssignmentMod of varId * expression
 | ExpLogicalOr of expression * expression
 | ExpLogicalAnd of expression * expression
 | ExpLogicalNot of expression
 | ExpLt of expression * expression
 | ExpLeq of expression * expression
 | ExpGt of expression * expression
 | ExpGeq of expression * expression
 | ExpEq of expression * expression
 | ExpNoteq of expression * expression
 | ExpLeftShift of expression * expression
 | ExpRightShift of expression * expression
 | ExpAdd of expression * expression
 | ExpSub of expression * expression
 | ExpMul of expression * expression
 | ExpDiv of expression * expression
 | ExpMod of expression * expression
 | ExpApp of expression * expression list
 | ExpAppUnit of expression
 | ExpNewObj of typeId * expression list
 | ExpNewObjUnit of typeId
 | ExpMethod of expression * varId
 | ExpBracket of expression

and literal =
   String of string
 | Char of char
 | Integer of int
 | Float of float
 | True
 | False
 | LUnit
 | AUnit

and fieldInit =
   FieldInitCons of varId * expression

