type token =
  | KW_bool
  | KW_tuple
  | KW_i8
  | KW_i16
  | KW_i32
  | KW_i64
  | KW_u8
  | KW_u16
  | KW_u32
  | KW_u64
  | KW_f32
  | KW_f64
  | KW_char
  | KW_This
  | KW_let
  | KW_mut
  | KW_fun
  | KW_interface
  | KW_extends
  | KW_impl
  | KW_for
  | KW_type
  | KW_as
  | KW_if
  | KW_else
  | KW_while
  | KW_return
  | KW_match
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
  | TOK_EOF
  | TOK_Ident of (string)
  | TOK_Char of (char)
  | TOK_Double of (float)
  | TOK_Integer of (int)
  | TOK_String of (string)
  | TOK_TypeId of (string)
  | TOK_VarId of (string)

val pCode_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.code list
val pCode :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.code
val pTypeT :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.typeT
val pType1 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.typeT
val pType2 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.typeT
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
val pConstructor :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.constructor
val pConstructor_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.constructor list
val pFields :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.fields
val pField :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.field
val pField_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.field list
val pTypedVar :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.typedVar
val pStatement :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.statement
val pIfCondtion :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.ifCondtion
val pForInit :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.forInit
val pForCondition :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.forCondition
val pForFinal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.forFinal
val pMatchBody :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.matchBody
val pMatchCase :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.matchCase
val pMatcher :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.matcher
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
val pMacro :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsDeeplang.macro