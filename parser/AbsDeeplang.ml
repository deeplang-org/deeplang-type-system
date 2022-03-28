(* OCaml module generated by the BNF converter *)


type typeId = TypeId of string
and varId = VarId of string
and code =
   Declares of declare
 | Defines of define
 | Statements of statement
 | Expressions of expression
 | Unit

and typeT =
   TypeArray of typeT * int
 | TypeList of typeT
 | TypeArrow of typeT * typeT
 | TypeBool
 | TypeTuple
 | TypeUnit
 | TypeI8
 | TypeI16
 | TypeI32
 | TypeI64
 | TypeU8
 | TypeU16
 | TypeU32
 | TypeU64
 | TypeF32
 | TypeF64
 | TypeChar
 | TypeThis
 | TypeX of typeId

and variable =
   Variables of varId

and declare =
   DecImmut of varId * typeT
 | DecMut of varId * typeT
 | DecFunc of varId * args * retType
 | InterfaceNoExt of interfaceName * methods
 | InterfaceExt of interfaceName * interfaceName list * methods

and args =
   ArgUnit
 | ArgUnit2
 | ArgExist of arg list

and arg =
   ArgCons of varId * typeT
 | Delegate of varId * typeT

and retType =
   RetUnit
 | RetExist of typeT

and interfaceName =
   InterfaceNames of typeId

and methods =
   InterfaceMethodUnit
 | InterfaceMethodExist of methodT list

and methodT =
   InterfaceMethod of varId * args * retType
 | ADTMethod of varId * args * retType * code list

and define =
   InterfaceImpl of interfaceName * typeT * methodT list
 | RawImpl of typeT * methodT list
 | DefFunc of varId * args * retType * code list
 | ADT of typeId * constructor list * methods
 | DefVar of typedVar * expression
 | DefType of typeId * args

and constructor =
   Constructors of typeId * fields

and fields =
   FieldUnit
 | FieldExist of field list

and field =
   FieldCons of varId * typeT

and typedVar =
   ImmutVar of varId * typeT
 | MutVar of varId * typeT

and statement =
   If of ifCondtion * code list
 | IfElse of ifCondtion * code list * code list
 | For of typedVar * expression * forCondition * forFinal * code list
 | While of forCondition * code list
 | Return of expression
 | Match of varId * matchBody

and ifCondtion =
   IfCondtions of expression

and forInit =
   ForInitUnit

and forCondition =
   ForConditions of expression

and forFinal =
   ForFinals of expression

and matchBody =
   MatchBodys of matchCase list

and matchCase =
   MatchCases of matcher * code list

and matcher =
   WildCardMatch
 | ConsMatch of constructor
 | VarMatch of typedVar
 | AsVarMatch of matcher * typedVar

and expression =
   ExpAssignment of variable * expression
 | ExpVar of variable
 | Literals of literal
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
 | ExpNewObj of typeId * expression list
 | ExpMethod of expression * variable
 | ExpBracket of expression

and literal =
   String of string
 | Integer of int
 | True
 | False

and macro =
   ArrayMatch of code list
