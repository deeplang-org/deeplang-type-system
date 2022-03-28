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

let rec showTypeId (AbsDeeplang.TypeId i) : showable = s2s "TypeId " >> showString i
let rec showVarId (AbsDeeplang.VarId i) : showable = s2s "VarId " >> showString i

let rec showCode (e : AbsDeeplang.code) : showable = match e with
       AbsDeeplang.Declares declare -> s2s "Declares" >> c2s ' ' >> c2s '(' >> showDeclare declare >> c2s ')'
  |    AbsDeeplang.Defines define -> s2s "Defines" >> c2s ' ' >> c2s '(' >> showDefine define >> c2s ')'
  |    AbsDeeplang.Statements statement -> s2s "Statements" >> c2s ' ' >> c2s '(' >> showStatement statement >> c2s ')'
  |    AbsDeeplang.Expressions expression -> s2s "Expressions" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'
  |    AbsDeeplang.Unit  -> s2s "Unit"


and showTypeT (e : AbsDeeplang.typeT) : showable = match e with
       AbsDeeplang.TypeArray (type', integer) -> s2s "TypeArray" >> c2s ' ' >> c2s '(' >> showTypeT type'  >> s2s ", " >>  showInt integer >> c2s ')'
  |    AbsDeeplang.TypeList type' -> s2s "TypeList" >> c2s ' ' >> c2s '(' >> showTypeT type' >> c2s ')'
  |    AbsDeeplang.TypeArrow (type'0, type') -> s2s "TypeArrow" >> c2s ' ' >> c2s '(' >> showTypeT type'0  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.TypeBool  -> s2s "TypeBool"
  |    AbsDeeplang.TypeTuple  -> s2s "TypeTuple"
  |    AbsDeeplang.TypeUnit  -> s2s "TypeUnit"
  |    AbsDeeplang.TypeI8  -> s2s "TypeI8"
  |    AbsDeeplang.TypeI16  -> s2s "TypeI16"
  |    AbsDeeplang.TypeI32  -> s2s "TypeI32"
  |    AbsDeeplang.TypeI64  -> s2s "TypeI64"
  |    AbsDeeplang.TypeU8  -> s2s "TypeU8"
  |    AbsDeeplang.TypeU16  -> s2s "TypeU16"
  |    AbsDeeplang.TypeU32  -> s2s "TypeU32"
  |    AbsDeeplang.TypeU64  -> s2s "TypeU64"
  |    AbsDeeplang.TypeF32  -> s2s "TypeF32"
  |    AbsDeeplang.TypeF64  -> s2s "TypeF64"
  |    AbsDeeplang.TypeChar  -> s2s "TypeChar"
  |    AbsDeeplang.TypeThis  -> s2s "TypeThis"
  |    AbsDeeplang.TypeX typeid -> s2s "TypeX" >> c2s ' ' >> c2s '(' >> showTypeId typeid >> c2s ')'


and showVariable (e : AbsDeeplang.variable) : showable = match e with
       AbsDeeplang.Variables varid -> s2s "Variables" >> c2s ' ' >> c2s '(' >> showVarId varid >> c2s ')'


and showDeclare (e : AbsDeeplang.declare) : showable = match e with
       AbsDeeplang.DecImmut (varid, type') -> s2s "DecImmut" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.DecMut (varid, type') -> s2s "DecMut" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.DecFunc (varid, args, rettype) -> s2s "DecFunc" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype >> c2s ')'
  |    AbsDeeplang.InterfaceNoExt (interfacename, methods) -> s2s "InterfaceNoExt" >> c2s ' ' >> c2s '(' >> showInterfaceName interfacename  >> s2s ", " >>  showMethods methods >> c2s ')'
  |    AbsDeeplang.InterfaceExt (interfacename, interfacenames, methods) -> s2s "InterfaceExt" >> c2s ' ' >> c2s '(' >> showInterfaceName interfacename  >> s2s ", " >>  showList showInterfaceName interfacenames  >> s2s ", " >>  showMethods methods >> c2s ')'


and showArgs (e : AbsDeeplang.args) : showable = match e with
       AbsDeeplang.ArgUnit  -> s2s "ArgUnit"
  |    AbsDeeplang.ArgUnit2  -> s2s "ArgUnit2"
  |    AbsDeeplang.ArgExist args -> s2s "ArgExist" >> c2s ' ' >> c2s '(' >> showList showArg args >> c2s ')'


and showArg (e : AbsDeeplang.arg) : showable = match e with
       AbsDeeplang.ArgCons (varid, type') -> s2s "ArgCons" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.Delegate (varid, type') -> s2s "Delegate" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'


and showRetType (e : AbsDeeplang.retType) : showable = match e with
       AbsDeeplang.RetUnit  -> s2s "RetUnit"
  |    AbsDeeplang.RetExist type' -> s2s "RetExist" >> c2s ' ' >> c2s '(' >> showTypeT type' >> c2s ')'


and showInterfaceName (e : AbsDeeplang.interfaceName) : showable = match e with
       AbsDeeplang.InterfaceNames typeid -> s2s "InterfaceNames" >> c2s ' ' >> c2s '(' >> showTypeId typeid >> c2s ')'


and showMethods (e : AbsDeeplang.methods) : showable = match e with
       AbsDeeplang.InterfaceMethodUnit  -> s2s "InterfaceMethodUnit"
  |    AbsDeeplang.InterfaceMethodExist methods -> s2s "InterfaceMethodExist" >> c2s ' ' >> c2s '(' >> showList showMethodT methods >> c2s ')'


and showMethodT (e : AbsDeeplang.methodT) : showable = match e with
       AbsDeeplang.InterfaceMethod (varid, args, rettype) -> s2s "InterfaceMethod" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype >> c2s ')'
  |    AbsDeeplang.ADTMethod (varid, args, rettype, codes) -> s2s "ADTMethod" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype  >> s2s ", " >>  showList showCode codes >> c2s ')'


and showDefine (e : AbsDeeplang.define) : showable = match e with
       AbsDeeplang.InterfaceImpl (interfacename, type', methods) -> s2s "InterfaceImpl" >> c2s ' ' >> c2s '(' >> showInterfaceName interfacename  >> s2s ", " >>  showTypeT type'  >> s2s ", " >>  showList showMethodT methods >> c2s ')'
  |    AbsDeeplang.RawImpl (type', methods) -> s2s "RawImpl" >> c2s ' ' >> c2s '(' >> showTypeT type'  >> s2s ", " >>  showList showMethodT methods >> c2s ')'
  |    AbsDeeplang.DefFunc (varid, args, rettype, codes) -> s2s "DefFunc" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showArgs args  >> s2s ", " >>  showRetType rettype  >> s2s ", " >>  showList showCode codes >> c2s ')'
  |    AbsDeeplang.ADT (typeid, constructors, methods) -> s2s "ADT" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showList showConstructor constructors  >> s2s ", " >>  showMethods methods >> c2s ')'
  |    AbsDeeplang.DefVar (typedvar, expression) -> s2s "DefVar" >> c2s ' ' >> c2s '(' >> showTypedVar typedvar  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.DefType (typeid, args) -> s2s "DefType" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showArgs args >> c2s ')'


and showConstructor (e : AbsDeeplang.constructor) : showable = match e with
       AbsDeeplang.Constructors (typeid, fields) -> s2s "Constructors" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showFields fields >> c2s ')'


and showFields (e : AbsDeeplang.fields) : showable = match e with
       AbsDeeplang.FieldUnit  -> s2s "FieldUnit"
  |    AbsDeeplang.FieldExist fields -> s2s "FieldExist" >> c2s ' ' >> c2s '(' >> showList showField fields >> c2s ')'


and showField (e : AbsDeeplang.field) : showable = match e with
       AbsDeeplang.FieldCons (varid, type') -> s2s "FieldCons" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'


and showTypedVar (e : AbsDeeplang.typedVar) : showable = match e with
       AbsDeeplang.ImmutVar (varid, type') -> s2s "ImmutVar" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'
  |    AbsDeeplang.MutVar (varid, type') -> s2s "MutVar" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showTypeT type' >> c2s ')'


and showStatement (e : AbsDeeplang.statement) : showable = match e with
       AbsDeeplang.If (ifcondtion, codes) -> s2s "If" >> c2s ' ' >> c2s '(' >> showIfCondtion ifcondtion  >> s2s ", " >>  showList showCode codes >> c2s ')'
  |    AbsDeeplang.IfElse (ifcondtion, codes0, codes) -> s2s "IfElse" >> c2s ' ' >> c2s '(' >> showIfCondtion ifcondtion  >> s2s ", " >>  showList showCode codes0  >> s2s ", " >>  showList showCode codes >> c2s ')'
  |    AbsDeeplang.For (typedvar, expression, forcondition, forfinal, codes) -> s2s "For" >> c2s ' ' >> c2s '(' >> showTypedVar typedvar  >> s2s ", " >>  showExpression expression  >> s2s ", " >>  showForCondition forcondition  >> s2s ", " >>  showForFinal forfinal  >> s2s ", " >>  showList showCode codes >> c2s ')'
  |    AbsDeeplang.While (forcondition, codes) -> s2s "While" >> c2s ' ' >> c2s '(' >> showForCondition forcondition  >> s2s ", " >>  showList showCode codes >> c2s ')'
  |    AbsDeeplang.Return expression -> s2s "Return" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'
  |    AbsDeeplang.Match (varid, matchbody) -> s2s "Match" >> c2s ' ' >> c2s '(' >> showVarId varid  >> s2s ", " >>  showMatchBody matchbody >> c2s ')'


and showIfCondtion (e : AbsDeeplang.ifCondtion) : showable = match e with
       AbsDeeplang.IfCondtions expression -> s2s "IfCondtions" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'


and showForInit (e : AbsDeeplang.forInit) : showable = match e with
       AbsDeeplang.ForInitUnit  -> s2s "ForInitUnit"


and showForCondition (e : AbsDeeplang.forCondition) : showable = match e with
       AbsDeeplang.ForConditions expression -> s2s "ForConditions" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'


and showForFinal (e : AbsDeeplang.forFinal) : showable = match e with
       AbsDeeplang.ForFinals expression -> s2s "ForFinals" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'


and showMatchBody (e : AbsDeeplang.matchBody) : showable = match e with
       AbsDeeplang.MatchBodys matchcases -> s2s "MatchBodys" >> c2s ' ' >> c2s '(' >> showList showMatchCase matchcases >> c2s ')'


and showMatchCase (e : AbsDeeplang.matchCase) : showable = match e with
       AbsDeeplang.MatchCases (matcher, codes) -> s2s "MatchCases" >> c2s ' ' >> c2s '(' >> showMatcher matcher  >> s2s ", " >>  showList showCode codes >> c2s ')'


and showMatcher (e : AbsDeeplang.matcher) : showable = match e with
       AbsDeeplang.WildCardMatch  -> s2s "WildCardMatch"
  |    AbsDeeplang.ConsMatch constructor -> s2s "ConsMatch" >> c2s ' ' >> c2s '(' >> showConstructor constructor >> c2s ')'
  |    AbsDeeplang.VarMatch typedvar -> s2s "VarMatch" >> c2s ' ' >> c2s '(' >> showTypedVar typedvar >> c2s ')'
  |    AbsDeeplang.AsVarMatch (matcher, typedvar) -> s2s "AsVarMatch" >> c2s ' ' >> c2s '(' >> showMatcher matcher  >> s2s ", " >>  showTypedVar typedvar >> c2s ')'


and showExpression (e : AbsDeeplang.expression) : showable = match e with
       AbsDeeplang.ExpAssignment (variable, expression) -> s2s "ExpAssignment" >> c2s ' ' >> c2s '(' >> showVariable variable  >> s2s ", " >>  showExpression expression >> c2s ')'
  |    AbsDeeplang.ExpVar variable -> s2s "ExpVar" >> c2s ' ' >> c2s '(' >> showVariable variable >> c2s ')'
  |    AbsDeeplang.Literals literal -> s2s "Literals" >> c2s ' ' >> c2s '(' >> showLiteral literal >> c2s ')'
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
  |    AbsDeeplang.ExpNewObj (typeid, expressions) -> s2s "ExpNewObj" >> c2s ' ' >> c2s '(' >> showTypeId typeid  >> s2s ", " >>  showList showExpression expressions >> c2s ')'
  |    AbsDeeplang.ExpMethod (expression, variable) -> s2s "ExpMethod" >> c2s ' ' >> c2s '(' >> showExpression expression  >> s2s ", " >>  showVariable variable >> c2s ')'
  |    AbsDeeplang.ExpBracket expression -> s2s "ExpBracket" >> c2s ' ' >> c2s '(' >> showExpression expression >> c2s ')'


and showLiteral (e : AbsDeeplang.literal) : showable = match e with
       AbsDeeplang.String string -> s2s "String" >> c2s ' ' >> c2s '(' >> showString string >> c2s ')'
  |    AbsDeeplang.Integer integer -> s2s "Integer" >> c2s ' ' >> c2s '(' >> showInt integer >> c2s ')'
  |    AbsDeeplang.True  -> s2s "True"
  |    AbsDeeplang.False  -> s2s "False"


and showMacro (e : AbsDeeplang.macro) : showable = match e with
       AbsDeeplang.ArrayMatch codes -> s2s "ArrayMatch" >> c2s ' ' >> c2s '(' >> showList showCode codes >> c2s ')'


