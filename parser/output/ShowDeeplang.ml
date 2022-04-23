(* show functions generated by the BNF converter *)


(* use string buffers for efficient string concatenations *)
type showable = Buffer.t -> unit

let show (s : showable) : string =
    let init_size = 16 in (* you may want to adjust this *)
    let b = Buffer.create init_size in
    s b;
    Buffer.contents b

let emptyS : showable = fun buf -> ()

let c2s (c:char) : showable = fun buf -> Buffer.add_char buf c
let s2s (s:string) : showable = fun buf -> Buffer.add_string buf s

let ( >> ) (s1 : showable) (s2 : showable) : showable = fun buf -> s1 buf; s2 buf

let showChar (c:char) : showable = fun buf ->
    Buffer.add_string buf ("'" ^ Char.escaped c ^ "'")

let showString (s:string) : showable = fun buf ->
    Buffer.add_string buf ("\"" ^ String.escaped s ^ "\"")

let showList (showFun : 'a -> showable) (xs : 'a list) : showable = fun buf ->
    let rec f ys = match ys with
        [] -> ()
      | [y] -> showFun y buf
      | y::ys -> showFun y buf; Buffer.add_string buf "; "; f ys
    in
        Buffer.add_char buf '[';
        f xs;
        Buffer.add_char buf ']'


let showInt (i:int) : showable = s2s (string_of_int i)
let showFloat (f:float) : showable = s2s (string_of_float f)

let rec showIF (AbsDeeplang.IF (_,i)) : showable = s2s "IF " >> showString i
let rec showELSE (AbsDeeplang.ELSE (_,i)) : showable = s2s "ELSE " >> showString i
let rec showWHILE (AbsDeeplang.WHILE (_,i)) : showable = s2s "WHILE " >> showString i
let rec showFOR (AbsDeeplang.FOR (_,i)) : showable = s2s "FOR " >> showString i
let rec showLET (AbsDeeplang.LET (_,i)) : showable = s2s "LET " >> showString i
let rec showFUN (AbsDeeplang.FUN (_,i)) : showable = s2s "FUN " >> showString i
let rec showMUT (AbsDeeplang.MUT (_,i)) : showable = s2s "MUT " >> showString i
let rec showINTERFACE (AbsDeeplang.INTERFACE (_,i)) : showable = s2s "INTERFACE " >> showString i
let rec showIMPL (AbsDeeplang.IMPL (_,i)) : showable = s2s "IMPL " >> showString i
let rec showAS (AbsDeeplang.AS (_,i)) : showable = s2s "AS " >> showString i
let rec showMATCH (AbsDeeplang.MATCH (_,i)) : showable = s2s "MATCH " >> showString i
let rec showTYPE (AbsDeeplang.TYPE (_,i)) : showable = s2s "TYPE " >> showString i
let rec showEXTENDS (AbsDeeplang.EXTENDS (_,i)) : showable = s2s "EXTENDS " >> showString i
let rec showTypeId (AbsDeeplang.TypeId ((_,i), n)) : showable = s2s "TypeId " >> showString i >> s2s "@" >> showInt n
let rec showBaseType (AbsDeeplang.BaseType (_,i)) : showable = s2s "BaseType " >> showString i
let rec showVarId (AbsDeeplang.VarId ((_,i), n)) : showable = s2s "VarId " >> showString i >> s2s "@" >> showInt n

let rec showCode (e : AbsDeeplang.code) : showable = match e with
       AbsDeeplang.Declares declare -> s2s "Declares" >> c2s ' ' >> c2s '(' >> showDeclare declare >> c2s ')'
  |    AbsDeeplang.Defines define -> s2s "Defines" >> c2s ' ' >> c2s '(' >> showDefine define >> c2s ')'
  |    AbsDeeplang.Statements statement -> s2s "Statements" >> c2s ' ' >> c2s '(' >> showStatement statement >> c2s ')'
  |    AbsDeeplang.Expressions expression -> s2s "Expressions" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'
  |    AbsDeeplang.Unit  -> s2s "Unit"


and showTypeT (e : AbsDeeplang.typeT) : showable = match e with
       AbsDeeplang.TypeFixLenArray (type', integer) -> s2s "TypeFixLenArray" >> c2s ' ' >> c2s '(' >> showTypeT type'  >> s2s ", " >>  showInt integer >> c2s ')'
  |    AbsDeeplang.TypeArrow (type'0, type') -> s2s "TypeArrow" >> c2s ' ' >> c2s '(' >> showTypeT type'0  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.TypeUnit1  -> s2s "TypeUnit1"
  |    AbsDeeplang.TypeUnit2  -> s2s "TypeUnit2"
  |    AbsDeeplang.TypeTuple types -> s2s "TypeTuple" >> c2s ' ' >> c2s '(' >> showList showTypeT types >> c2s ')'
  |    AbsDeeplang.TypePrimitive basetype -> s2s "TypePrimitive" >> c2s ' ' >> c2s '(' >> showBaseType basetype >> c2s ')'
  |    AbsDeeplang.TypeX typeid -> s2s "TypeX" >> c2s ' ' >> c2s '(' >> showTypeId typeid >> c2s ')'


and showVariable (e : AbsDeeplang.variable) : showable = match e with
       AbsDeeplang.Variables varid -> s2s "Variables" >> c2s ' ' >> c2s '(' >> showVarId varid >> c2s ')'


and showDeclare (e : AbsDeeplang.declare) : showable = match e with
       AbsDeeplang.DecImmut (let', varid, type') -> s2s "DecImmut" >> c2s ' ' >> c2s '(' >> showLET let'  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.DecMut (let', mut, varid, type') -> s2s "DecMut" >> c2s ' ' >> c2s '(' >> showLET let'  >> s2s ", " >>  showMUT mut  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.DecFunc (fun', varid, args, rettype) -> s2s "DecFunc" >> c2s ' ' >> c2s '(' >> showFUN fun'  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype >> c2s ')'
  |    AbsDeeplang.InterfaceNoExt (interface, interfacename, methods) -> s2s "InterfaceNoExt" >> c2s ' ' >> c2s '(' >> showINTERFACE interface  >> s2s ", " >>  showInterfaceName interfacename  >> s2s ", " >>  showMethods methods >> c2s ')'
  |    AbsDeeplang.InterfaceExt (interface, interfacename, extends, interfacenames, methods) -> s2s "InterfaceExt" >> c2s ' ' >> c2s '(' >> showINTERFACE interface  >> s2s ", " >>  showInterfaceName interfacename  >> s2s ", " >>  showEXTENDS extends  >> s2s ", " >>  showList showInterfaceName interfacenames  >> s2s ", " >>  showMethods methods >> c2s ')'


and showArgs (e : AbsDeeplang.args) : showable = match e with
       AbsDeeplang.ArgUnit  -> s2s "ArgUnit"
  |    AbsDeeplang.ArgUnit2  -> s2s "ArgUnit2"
  |    AbsDeeplang.ArgExist args -> s2s "ArgExist" >> c2s ' ' >> c2s '(' >> showList showArg args >> c2s ')'


and showArg (e : AbsDeeplang.arg) : showable = match e with
       AbsDeeplang.ArgCons (varid, type') -> s2s "ArgCons" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'


and showRetType (e : AbsDeeplang.retType) : showable = match e with
       AbsDeeplang.RetUnit  -> s2s "RetUnit"
  |    AbsDeeplang.RetExist type' -> s2s "RetExist" >> c2s ' ' >> c2s '(' >> showTypeT type' >> c2s ')'


and showInterfaceName (e : AbsDeeplang.interfaceName) : showable = match e with
       AbsDeeplang.InterfaceNames typeid -> s2s "InterfaceNames" >> c2s ' ' >> c2s '(' >> showTypeId typeid >> c2s ')'


and showMethods (e : AbsDeeplang.methods) : showable = match e with
       AbsDeeplang.InterfaceMethodUnit  -> s2s "InterfaceMethodUnit"
  |    AbsDeeplang.InterfaceMethodExist methods -> s2s "InterfaceMethodExist" >> c2s ' ' >> c2s '(' >> showList showMethodT methods >> c2s ')'


and showMethodT (e : AbsDeeplang.methodT) : showable = match e with
       AbsDeeplang.InterfaceMethod (fun', varid, args, rettype) -> s2s "InterfaceMethod" >> c2s ' ' >> c2s '(' >> showFUN fun'  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype >> c2s ')'
  |    AbsDeeplang.ADTMethod (fun', varid, args, rettype, codes) -> s2s "ADTMethod" >> c2s ' ' >> c2s '(' >> showFUN fun'  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype  >> s2s ", " >>  showList showCode codes >> c2s ')'


and showDefine (e : AbsDeeplang.define) : showable = match e with
       AbsDeeplang.DefFunc function' -> s2s "DefFunc" >> c2s ' ' >> c2s '(' >> showFunctionT function' >> c2s ')'
  |    AbsDeeplang.ADT (type', typeid, constructors) -> s2s "ADT" >> c2s ' ' >> c2s '(' >> showTYPE type'  >> s2s ", " >>  showTypeId typeid  >> s2s ", " >>  showList showConstructor constructors >> c2s ')'
  |    AbsDeeplang.Struct (type', typeid, structfields) -> s2s "Struct" >> c2s ' ' >> c2s '(' >> showTYPE type'  >> s2s ", " >>  showTypeId typeid  >> s2s ", " >>  showList showStructField structfields >> c2s ')'
  |    AbsDeeplang.DefVar (let', typedvar, expression) -> s2s "DefVar" >> c2s ' ' >> c2s '(' >> showLET let'  >> s2s ", " >>  showTypedVar typedvar  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.DefType (type', typeid, args) -> s2s "DefType" >> c2s ' ' >> c2s '(' >> showTYPE type'  >> s2s ", " >>  showTypeId typeid  >> s2s ", " >>  showArgs args >> c2s ')'
  |    AbsDeeplang.InterfaceImpl (impl, interfacename, for', type', functions) -> s2s "InterfaceImpl" >> c2s ' ' >> c2s '(' >> showIMPL impl  >> s2s ", " >>  showInterfaceName interfacename  >> s2s ", " >>  showFOR for'  >> s2s ", " >>  showTypeT type'  >> s2s ", " >>  showFunctions functions >> c2s ')'
  |    AbsDeeplang.RawImpl (impl, type', functions) -> s2s "RawImpl" >> c2s ' ' >> c2s '(' >> showIMPL impl  >> s2s ", " >>  showTypeT type'  >> s2s ", " >>  showFunctions functions >> c2s ')'


and showFunctionT (e : AbsDeeplang.functionT) : showable = match e with
       AbsDeeplang.FuncUnit (fun', varid, args, rettype) -> s2s "FuncUnit" >> c2s ' ' >> c2s '(' >> showFUN fun'  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype >> c2s ')'
  |    AbsDeeplang.Func (fun', varid, args, rettype, codes) -> s2s "Func" >> c2s ' ' >> c2s '(' >> showFUN fun'  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype  >> s2s ", " >>  showList showCode codes >> c2s ')'


and showConstructor (e : AbsDeeplang.constructor) : showable = match e with
       AbsDeeplang.Constructors (typeid, fields) -> s2s "Constructors" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showList showField fields >> c2s ')'


and showField (e : AbsDeeplang.field) : showable = match e with
       AbsDeeplang.FieldCons (varid, type') -> s2s "FieldCons" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'


and showStructField (e : AbsDeeplang.structField) : showable = match e with
       AbsDeeplang.BasicStructField field -> s2s "BasicStructField" >> c2s ' ' >> c2s '(' >> showField field >> c2s ')'
  |    AbsDeeplang.DelegateStructField (as', field) -> s2s "DelegateStructField" >> c2s ' ' >> c2s '(' >> showAS as'  >> s2s ", " >>  showField field >> c2s ')'


and showTypedVar (e : AbsDeeplang.typedVar) : showable = match e with
       AbsDeeplang.ImmutVar (varid, type') -> s2s "ImmutVar" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.MutVar (mut, varid, type') -> s2s "MutVar" >> c2s ' ' >> c2s '(' >> showMUT mut  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'


and showFunctions (e : AbsDeeplang.functions) : showable = match e with
       AbsDeeplang.FunctionsUnit  -> s2s "FunctionsUnit"
  |    AbsDeeplang.FunctionsMany functions -> s2s "FunctionsMany" >> c2s ' ' >> c2s '(' >> showList showFunctionT functions >> c2s ')'


and showStatement (e : AbsDeeplang.statement) : showable = match e with
       AbsDeeplang.Block statements -> s2s "Block" >> c2s ' ' >> c2s '(' >> showList showStatement statements >> c2s ')'
  |    AbsDeeplang.DefVarSt (let', typedvar, expression) -> s2s "DefVarSt" >> c2s ' ' >> c2s '(' >> showLET let'  >> s2s ", " >>  showTypedVar typedvar  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.DefTypeSt (type', typeid, args) -> s2s "DefTypeSt" >> c2s ' ' >> c2s '(' >> showTYPE type'  >> s2s ", " >>  showTypeId typeid  >> s2s ", " >>  showArgs args >> c2s ')'
  |    AbsDeeplang.ExprSt expression -> s2s "ExprSt" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'
  |    AbsDeeplang.Return expression -> s2s "Return" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'
  |    AbsDeeplang.If (if', expression, statements, elsebody) -> s2s "If" >> c2s ' ' >> c2s '(' >> showIF if'  >> s2s ", " >>  showExpression expression  >> s2s ", " >>  showList showStatement statements  >> s2s ", " >>  showElseBody elsebody >> c2s ')'
  |    AbsDeeplang.For (for', matcher, expression, statements) -> s2s "For" >> c2s ' ' >> c2s '(' >> showFOR for'  >> s2s ", " >>  showMatcher matcher  >> s2s ", " >>  showExpression expression  >> s2s ", " >>  showList showStatement statements >> c2s ')'
  |    AbsDeeplang.While (while', expression, statements) -> s2s "While" >> c2s ' ' >> c2s '(' >> showWHILE while'  >> s2s ", " >>  showExpression expression  >> s2s ", " >>  showList showStatement statements >> c2s ')'
  |    AbsDeeplang.Match (match', varid, matchbody) -> s2s "Match" >> c2s ' ' >> c2s '(' >> showMATCH match'  >> s2s ", " >>  showVarId varid  >> s2s ", " >>  showMatchBody matchbody >> c2s ')'


and showElseBody (e : AbsDeeplang.elseBody) : showable = match e with
       AbsDeeplang.NoElse  -> s2s "NoElse"
  |    AbsDeeplang.Elif (else', if', expression, statements, elsebody) -> s2s "Elif" >> c2s ' ' >> c2s '(' >> showELSE else'  >> s2s ", " >>  showIF if'  >> s2s ", " >>  showExpression expression  >> s2s ", " >>  showList showStatement statements  >> s2s ", " >>  showElseBody elsebody >> c2s ')'
  |    AbsDeeplang.Else (else', statements) -> s2s "Else" >> c2s ' ' >> c2s '(' >> showELSE else'  >> s2s ", " >>  showList showStatement statements >> c2s ')'


and showMatchBody (e : AbsDeeplang.matchBody) : showable = match e with
       AbsDeeplang.MatchBodys matchcases -> s2s "MatchBodys" >> c2s ' ' >> c2s '(' >> showList showMatchCase matchcases >> c2s ')'


and showMatchCase (e : AbsDeeplang.matchCase) : showable = match e with
       AbsDeeplang.MatchCases (matcher, statements) -> s2s "MatchCases" >> c2s ' ' >> c2s '(' >> showMatcher matcher  >> s2s ", " >>  showList showStatement statements >> c2s ')'


and showMatcher (e : AbsDeeplang.matcher) : showable = match e with
       AbsDeeplang.WildCardMatch  -> s2s "WildCardMatch"
  |    AbsDeeplang.ConsMatchUnit typeid -> s2s "ConsMatchUnit" >> c2s ' ' >> c2s '(' >> showTypeId typeid >> c2s ')'
  |    AbsDeeplang.ConsMatch (typeid, matcher) -> s2s "ConsMatch" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showMatcher matcher >> c2s ')'
  |    AbsDeeplang.TypelessVarMatch varid -> s2s "TypelessVarMatch" >> c2s ' ' >> c2s '(' >> showVarId varid >> c2s ')'
  |    AbsDeeplang.UnitMatch  -> s2s "UnitMatch"
  |    AbsDeeplang.TupleMatch matchers -> s2s "TupleMatch" >> c2s ' ' >> c2s '(' >> showList showMatcher matchers >> c2s ')'
  |    AbsDeeplang.LiteralMatch literal -> s2s "LiteralMatch" >> c2s ' ' >> c2s '(' >> showLiteral literal >> c2s ')'
  |    AbsDeeplang.FieldMatchUnit typeid -> s2s "FieldMatchUnit" >> c2s ' ' >> c2s '(' >> showTypeId typeid >> c2s ')'
  |    AbsDeeplang.FieldMatch (typeid, fieldmatchers) -> s2s "FieldMatch" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showList showFieldMatcher fieldmatchers >> c2s ')'


and showFieldMatcher (e : AbsDeeplang.fieldMatcher) : showable = match e with
       AbsDeeplang.FieldMatchers (varid, matcher) -> s2s "FieldMatchers" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showMatcher matcher >> c2s ')'


and showExpression (e : AbsDeeplang.expression) : showable = match e with
       AbsDeeplang.ExpVar variable -> s2s "ExpVar" >> c2s ' ' >> c2s '(' >> showVariable variable >> c2s ')'
  |    AbsDeeplang.Literals literal -> s2s "Literals" >> c2s ' ' >> c2s '(' >> showLiteral literal >> c2s ')'
  |    AbsDeeplang.Tuples expressions -> s2s "Tuples" >> c2s ' ' >> c2s '(' >> showList showExpression expressions >> c2s ')'
  |    AbsDeeplang.StructInit (typeid, fieldinits) -> s2s "StructInit" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showList showFieldInit fieldinits >> c2s ')'
  |    AbsDeeplang.ExpAssignment (variable, expression) -> s2s "ExpAssignment" >> c2s ' ' >> c2s '(' >> showVariable variable  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpAssignmentPlus (variable, expression) -> s2s "ExpAssignmentPlus" >> c2s ' ' >> c2s '(' >> showVariable variable  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpAssignmentMinus (variable, expression) -> s2s "ExpAssignmentMinus" >> c2s ' ' >> c2s '(' >> showVariable variable  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpAssignmentMul (variable, expression) -> s2s "ExpAssignmentMul" >> c2s ' ' >> c2s '(' >> showVariable variable  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpAssignmentDiv (variable, expression) -> s2s "ExpAssignmentDiv" >> c2s ' ' >> c2s '(' >> showVariable variable  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpAssignmentMod (variable, expression) -> s2s "ExpAssignmentMod" >> c2s ' ' >> c2s '(' >> showVariable variable  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpLogicalOr (expression0, expression) -> s2s "ExpLogicalOr" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpLogicalAnd (expression0, expression) -> s2s "ExpLogicalAnd" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpLogicalNot expression -> s2s "ExpLogicalNot" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpLt (expression0, expression) -> s2s "ExpLt" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpLeq (expression0, expression) -> s2s "ExpLeq" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpGt (expression0, expression) -> s2s "ExpGt" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpGeq (expression0, expression) -> s2s "ExpGeq" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpEq (expression0, expression) -> s2s "ExpEq" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpNoteq (expression0, expression) -> s2s "ExpNoteq" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpLeftShift (expression0, expression) -> s2s "ExpLeftShift" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpRightShift (expression0, expression) -> s2s "ExpRightShift" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpAdd (expression0, expression) -> s2s "ExpAdd" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpSub (expression0, expression) -> s2s "ExpSub" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpMul (expression0, expression) -> s2s "ExpMul" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpDiv (expression0, expression) -> s2s "ExpDiv" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpMod (expression0, expression) -> s2s "ExpMod" >> c2s ' ' >> c2s '(' >> showExpression expression0  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpApp (expression, expressions) -> s2s "ExpApp" >> c2s ' ' >> c2s '(' >> showExpression expression  >> s2s ", " >>  showList showExpression expressions >> c2s ')'
  |    AbsDeeplang.ExpAppUnit expression -> s2s "ExpAppUnit" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpNewObj (typeid, expressions) -> s2s "ExpNewObj" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showList showExpression expressions >> c2s ')'
  |    AbsDeeplang.ExpNewObjUnit typeid -> s2s "ExpNewObjUnit" >> c2s ' ' >> c2s '(' >> showTypeId typeid >> c2s ')'
  |    AbsDeeplang.ExpMethod (expression, variable) -> s2s "ExpMethod" >> c2s ' ' >> c2s '(' >> showExpression expression  >> s2s ", " >>  showVariable variable >> c2s ')'
  |    AbsDeeplang.ExpBracket expression -> s2s "ExpBracket" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'


and showLiteral (e : AbsDeeplang.literal) : showable = match e with
       AbsDeeplang.String string -> s2s "String" >> c2s ' ' >> c2s '(' >> showString string >> c2s ')'
  |    AbsDeeplang.Char char -> s2s "Char" >> c2s ' ' >> c2s '(' >> showChar char >> c2s ')'
  |    AbsDeeplang.Integer integer -> s2s "Integer" >> c2s ' ' >> c2s '(' >> showInt integer >> c2s ')'
  |    AbsDeeplang.Float double -> s2s "Float" >> c2s ' ' >> c2s '(' >> showFloat double >> c2s ')'
  |    AbsDeeplang.True  -> s2s "True"
  |    AbsDeeplang.False  -> s2s "False"
  |    AbsDeeplang.LUnit  -> s2s "LUnit"


and showFieldInit (e : AbsDeeplang.fieldInit) : showable = match e with
       AbsDeeplang.FieldInitCons (varid, expression) -> s2s "FieldInitCons" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showExpression expression >> c2s ')'



