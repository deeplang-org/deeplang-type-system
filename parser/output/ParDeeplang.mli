type token =
  | KW_return
  | KW_true
  | KW_false
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
