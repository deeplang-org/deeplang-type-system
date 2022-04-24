type token =
  | KW_return
  | KW_true
  | KW_false
  | KW_new
  | SYMB1
  | SYMB2
  | SYMB3
  | SYMB4
  | SYMB5
  | SYMB6
  | SYMB7
  | SYMB8
  | SYMB9
  | SYMB10
  | SYMB11
  | SYMB12
  | SYMB13
  | SYMB14
  | SYMB15
  | SYMB16
  | SYMB17
  | SYMB18
  | SYMB19
  | SYMB20
  | SYMB21
  | SYMB22
  | SYMB23
  | SYMB24
  | SYMB25
  | SYMB26
  | SYMB27
  | SYMB28
  | SYMB29
  | SYMB30
  | SYMB31
  | SYMB32
  | SYMB33
  | SYMB34
  | SYMB35
  | SYMB36
  | SYMB37
  | TOK_EOF
  | TOK_Ident of (string)
  | TOK_Char of (char)
  | TOK_Double of (float)
  | TOK_Integer of (int)
  | TOK_String of (string)
  | TOK_IF of ((int * int) * string)
  | TOK_ELSE of ((int * int) * string)
  | TOK_WHILE of ((int * int) * string)
  | TOK_FOR of ((int * int) * string)
  | TOK_IN of ((int * int) * string)
  | TOK_LET of ((int * int) * string)
  | TOK_FUN of ((int * int) * string)
  | TOK_MUT of ((int * int) * string)
  | TOK_INTERFACE of ((int * int) * string)
  | TOK_IMPL of ((int * int) * string)
  | TOK_AS of ((int * int) * string)
  | TOK_MATCH of ((int * int) * string)
  | TOK_TYPE of ((int * int) * string)
  | TOK_EXTENDS of ((int * int) * string)
  | TOK_TypeId of ((int * int) * string)
  | TOK_BaseType of ((int * int) * string)
  | TOK_VarId of ((int * int) * string)

val pCode_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.code list
val pCode :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.code
val pTypeT :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.typeT
val pTypeT_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.typeT list
val pVariable :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.variable
val pVariable_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.variable list
val pDeclare :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.declare
val pArgs :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.args
val pArg :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.arg
val pArg_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.arg list
val pRetType :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.retType
val pInterfaceName :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.interfaceName
val pInterfaceName_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.interfaceName list
val pMethods :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.methods
val pMethodT :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.methodT
val pMethodT_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.methodT list
val pDefine :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.define
val pFunctionT :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.functionT
val pConstructor :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.constructor
val pConstructor_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.constructor list
val pField :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.field
val pField_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.field list
val pStructField :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.structField
val pStructField_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.structField list
val pRHS :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.rHS
val pFunctions :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.functions
val pFunctionT_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.functionT list
val pStatement :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.statement
val pStatement_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.statement list
val pElseBody :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.elseBody
val pMatchBody :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.matchBody
val pMatchCase :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.matchCase
val pMatcher :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.matcher
val pTypedMatcher :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.typedMatcher
val pTypelessMatcher :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.typelessMatcher
val pMatcher_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.matcher list
val pFieldMatcher_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.fieldMatcher list
val pFieldMatcher :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.fieldMatcher
val pMatchCase_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.matchCase list
val pExpression :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression1 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression2 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression3 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression4 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression5 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression6 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression7 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression8 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression9 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression10 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression11 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression12 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression13 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression
val pExpression_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.expression list
val pLiteral :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.literal
val pFieldInit_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.fieldInit list
val pFieldInit :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.fieldInit
