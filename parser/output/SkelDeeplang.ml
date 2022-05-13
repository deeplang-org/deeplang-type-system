module SkelDeeplang = struct

(* OCaml module generated by the BNF converter *)

open AbsDeeplang

type result = string

let failure x = failwith "Undefined case." (* x discarded *)

let rec transIF (x : iF) : result = match x with
    IF string -> failure x


and transELSE (x : eLSE) : result = match x with
    ELSE string -> failure x


and transWHILE (x : wHILE) : result = match x with
    WHILE string -> failure x


and transFOR (x : fOR) : result = match x with
    FOR string -> failure x


and transIN (x : iN) : result = match x with
    IN string -> failure x


and transLET (x : lET) : result = match x with
    LET string -> failure x


and transFUN (x : fUN) : result = match x with
    FUN string -> failure x


and transMUT (x : mUT) : result = match x with
    MUT string -> failure x


and transINTERFACE (x : iNTERFACE) : result = match x with
    INTERFACE string -> failure x


and transIMPL (x : iMPL) : result = match x with
    IMPL string -> failure x


and transAS (x : aS) : result = match x with
    AS string -> failure x


and transMATCH (x : mATCH) : result = match x with
    MATCH string -> failure x


and transTYPE (x : tYPE) : result = match x with
    TYPE string -> failure x


and transEXTENDS (x : eXTENDS) : result = match x with
    EXTENDS string -> failure x


and transTypeId (x : typeId) : result = match x with
    TypeId string -> failure x


and transBaseType (x : baseType) : result = match x with
    BaseType string -> failure x


and transVarId (x : varId) : result = match x with
    VarId string -> failure x


and transCode (x : code) : result = match x with
    Declares declare -> failure x
  | Defines define -> failure x
  | Statements statement -> failure x
  | Expressions expression -> failure x
  | Unit  -> failure x


and transType (x : typeT) : result = match x.typeTShape with
    TypeFixLenArray (type', integer) -> failure x
  | TypeArrow (type'0, type') -> failure x
  | TypeUnit  -> failure x
  | TypeTuple types -> failure x
  | TypePrimitive basetype -> failure x
  | TypeX typeid -> failure x


and transMVarId (x : mVarId) : result = match x.mVarIdShape with
    _ -> failure x


and transDeclare (x : declare) : result = match x.declareShape with
    DecFunc (fun', varid, args, rettype) -> failure x
  | InterfaceNoExt (interface, interfacename, methods) -> failure x
  | InterfaceExt (interface, interfacename, interfacenames, methods) -> failure x


and transArgs (x : args) : result = match x with
    ArgUnit  -> failure x
  | ArgExist args -> failure x


and transArg (x : arg) : result = match x with
    ArgCons (varid, type') -> failure x


and transRetType (x : retType) : result = match x with
    RetUnit  -> failure x
  | RetExist type' -> failure x


and transInterfaceName (x : interfaceName) : result = match x with
    InterfaceNames typeid -> failure x


and transMethod (x : methodT) : result = match x with
    InterfaceMethod (fun', varid, args, rettype) -> failure x


and transDefine (x : define) : result = match x.defineShape with
    DefFunc function' -> failure x
  | ADT (type', typeid, constructors) -> failure x
  | Struct (type', typeid, structfields) -> failure x
  | DefVar (let', mutflag, typedmatcher, rhs) -> failure x
  | DefType (type', typeid, args) -> failure x
  | InterfaceImpl (impl, interfacename, for', type', functions) -> failure x
  | RawImpl (impl, type', functions) -> failure x


and transFunction (x : functionT) : result = match x with
    Func (fun', varid, args, rettype, statements) -> failure x


and transConstructor (x : constructor) : result = match x.constructorShape with
    (typeid, fields) -> failure x


and transField (x : field) : result = match x.fieldShape with
    (varid, type') -> failure x


and transStructField (x : structField) : result = match x.structFieldShape with
    (_, field) -> failure x


and transRHS (x : rHS) : result = match x with
    DefRHS expression -> failure x
  | NilRHS  -> failure x


and transStatement (x : statement) : result = match x.statementShape with
    Block statements -> failure x
  | DefVarSt (let', mutflag, typedmatcher, rhs) -> failure x
  | ExprSt expression -> failure x
  | Return expression -> failure x
  | If (if', expression, statements, elsebody) -> failure x
  | For (for', matcher, in', expression, statements) -> failure x
  | While (while', expression, statements) -> failure x
  | Match (match', varid, matchbody) -> failure x


and transMatchBody (x : matchBody) : result = match x with
    MatchBodys matchcases -> failure x


and transMatchCase (x : matchCase) : result = match x with
    MatchCases (matcher, statements) -> failure x


and transMatcher (x : matcher) : result = match x with
    TypedMatchers typedmatcher -> failure x
  | TypelessMatchers typelessmatcher -> failure x
  | AsVarMatch (matcher, as', mvarid) -> failure x


and transTypedMatcher (x : typedMatcher) : result = match x with
    Typed (typelessmatcher, type') -> failure x


and transTypelessMatcher (x : typelessMatcher) : result = match x with
    WildCardMatch  -> failure x
  | ConsMatchUnit typeid -> failure x
  | ConsMatch (typeid, matcher) -> failure x
  | TypelessVarMatch mvarid -> failure x
  | UnitMatch  -> failure x
  | TupleMatch matchers -> failure x
  | LiteralMatch literal -> failure x
  | FieldMatchUnit typeid -> failure x
  | FieldMatch (typeid, fieldmatchers) -> failure x


and transFieldMatcher (x : fieldMatcher) : result = match x with
    FieldMatchers (varid, typelessmatcher) -> failure x


and transExpression (x : expression) : result = match x with
    ExpVar matcher -> failure x
  | Literals literal -> failure x
  | Tuples expressions -> failure x
  | Array expressions -> failure x
  | StructInit (typeid, fieldinits) -> failure x
  | ExpAssignment (varid, expression) -> failure x
  | ExpAssignmentPlus (varid, expression) -> failure x
  | ExpAssignmentMinus (varid, expression) -> failure x
  | ExpAssignmentMul (varid, expression) -> failure x
  | ExpAssignmentDiv (varid, expression) -> failure x
  | ExpAssignmentMod (varid, expression) -> failure x
  | ExpLogicalOr (expression0, expression) -> failure x
  | ExpLogicalAnd (expression0, expression) -> failure x
  | ExpLogicalNot expression -> failure x
  | ExpLt (expression0, expression) -> failure x
  | ExpLeq (expression0, expression) -> failure x
  | ExpGt (expression0, expression) -> failure x
  | ExpGeq (expression0, expression) -> failure x
  | ExpEq (expression0, expression) -> failure x
  | ExpNoteq (expression0, expression) -> failure x
  | ExpLeftShift (expression0, expression) -> failure x
  | ExpRightShift (expression0, expression) -> failure x
  | ExpAdd (expression0, expression) -> failure x
  | ExpSub (expression0, expression) -> failure x
  | ExpMul (expression0, expression) -> failure x
  | ExpDiv (expression0, expression) -> failure x
  | ExpMod (expression0, expression) -> failure x
  | ExpApp (expression, expressions) -> failure x
  | ExpAppUnit expression -> failure x
  | ExpNewObj (typeid, expressions) -> failure x
  | ExpNewObjUnit typeid -> failure x
  | ExpMethod (expression, varid) -> failure x
  | ExpBracket expression -> failure x


and transLiteral (x : literal) : result = match x with
    String string -> failure x
  | Char char -> failure x
  | Integer integer -> failure x
  | Float double -> failure x
  | True  -> failure x
  | False  -> failure x
  | LUnit  -> failure x
  | AUnit  -> failure x


and transFieldInit (x : fieldInit) : result = match x with
    FieldInitCons (varid, expression) -> failure x



end
