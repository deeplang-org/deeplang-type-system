(* This ocamllex file was machine-generated by the BNF converter *)

(* preamble *)
{
open ParDeeplang
open Lexing

let symbol_table = Hashtbl.create 38
let _ = List.iter (fun (kwd, tok) -> Hashtbl.add symbol_table kwd tok)
                  [(";", SYMB1);("[", SYMB2);("]", SYMB3);("->", SYMB4);("()", SYMB5);("(", SYMB6);(")", SYMB7);(",", SYMB8);(":", SYMB9);("{}", SYMB10);("{", SYMB11);("}", SYMB12);("=", SYMB13);("=>", SYMB14);("_", SYMB15);("[]", SYMB16);("+=", SYMB17);("-=", SYMB18);("*=", SYMB19);("/=", SYMB20);("%=", SYMB21);("||", SYMB22);("&&", SYMB23);("!", SYMB24);("<", SYMB25);("<=", SYMB26);(">", SYMB27);(">=", SYMB28);("==", SYMB29);("!=", SYMB30);("<<", SYMB31);(">>", SYMB32);("+", SYMB33);("-", SYMB34);("*", SYMB35);("/", SYMB36);("%", SYMB37);(".", SYMB38)]

let resword_table = Hashtbl.create 3
let _ = List.iter (fun (kwd, tok) -> Hashtbl.add resword_table kwd tok)
                  [("return", KW_return);("true", KW_true);("false", KW_false)]

let unescapeInitTail (s:string) : string =
  let rec unesc s = match s with
      '\\'::c::cs when List.mem c ['\"'; '\\'; '\''] -> c :: unesc cs
    | '\\'::'n'::cs  -> '\n' :: unesc cs
    | '\\'::'t'::cs  -> '\t' :: unesc cs
    | '\\'::'r'::cs  -> '\r' :: unesc cs
    | '\"'::[]    -> []
    | c::cs      -> c :: unesc cs
    | _         -> []
  (* explode/implode from caml FAQ *)
  in let explode (s : string) : char list =
      let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
      exp (String.length s - 1) []
  in let implode (l : char list) : string =
      let res = Buffer.create (List.length l) in
      List.iter (Buffer.add_char res) l;
      Buffer.contents res
  in implode (unesc (List.tl (explode s)))

let incr_lineno (lexbuf:Lexing.lexbuf) : unit =
    let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with
            pos_lnum = pos.pos_lnum + 1;
            pos_bol = pos.pos_cnum;
        }
}

(* BNFC character classes *)
let _letter = ['a'-'z' 'A'-'Z' '\192' - '\255'] # ['\215' '\247']    (*  isolatin1 letter FIXME *)
let _upper  = ['A'-'Z' '\192'-'\221'] # '\215'      (*  capital isolatin1 letter FIXME *)
let _lower  = ['a'-'z' '\222'-'\255'] # '\247'      (*  small isolatin1 letter FIXME *)
let _digit  = ['0'-'9']                             (*  _digit *)
let _idchar = _letter | _digit | ['_' '\'']         (*  identifier character *)
let _universal = _                                  (* universal: any character *)

(* reserved words consisting of special symbols *)
let rsyms = ";" | "[" | "]" | "->" | "()" | "(" | ")" | "," | ":" | "{}" | "{" | "}" | "=" | "=>" | "_" | "[]" | "+=" | "-=" | "*=" | "/=" | "%=" | "||" | "&&" | "!" | "<" | "<=" | ">" | ">=" | "==" | "!=" | "<<" | ">>" | "+" | "-" | "*" | "/" | "%" | "."
(* user-defined token types *)
let iF = "if"
let eLSE = "else"
let wHILE = "while"
let fOR = "for"
let iN = "in"
let lET = "let"
let fUN = "fun"
let mUT = "mut"
let iNTERFACE = "interface"
let iMPL = "impl"
let aS = "as"
let mATCH = "match"
let tYPE = "type"
let eXTENDS = "extends"
let typeId = _upper ('_' | (_digit | _letter)) *
let baseType = "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64" | "char" | "This" | "bool"
let varId = ('_' | _lower)('_' | (_digit | _letter)) *

(* lexing rules *)
rule token =
  parse "//" (_ # '\n')*
                { token lexbuf }
      | "/*" [^ '*']* '*' ([^ '*' '/'][^ '*']* '*' | '*')* '/'
                { token lexbuf }
      | rsyms   { let x = lexeme lexbuf in try Hashtbl.find symbol_table x with Not_found -> failwith ("internal lexer error: reserved symbol " ^ x ^ " not found in hashtable") }
      | iF      { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_IF ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | eLSE    { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_ELSE ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | wHILE   { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_WHILE ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | fOR     { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_FOR ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | iN      { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_IN ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | lET     { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_LET ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | fUN     { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_FUN ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | mUT     { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_MUT ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | iNTERFACE
                { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_INTERFACE ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | iMPL    { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_IMPL ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | aS      { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_AS ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | mATCH   { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_MATCH ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | tYPE    { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_TYPE ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | eXTENDS { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_EXTENDS ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | typeId  { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_TypeId ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | baseType
                { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_BaseType ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | varId   { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_VarId ((lexeme_start lexbuf, lexeme_end lexbuf), l) }
      | _letter _idchar*
                { let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_Ident l }
      | _digit+ { TOK_Integer (int_of_string (lexeme lexbuf)) }
      | _digit+ '.' _digit+ ('e' ('-')? _digit+)?
                { TOK_Double (float_of_string (lexeme lexbuf)) }
      | '\"' (([^ '\"' '\\' '\n']) | ('\\' ('\"' | '\\' | '\'' | 'n' | 't' | 'r')))* '\"'
                { TOK_String (unescapeInitTail (lexeme lexbuf)) }
      | '\'' (([^ '\'' '\\']) | ('\\' ('\\' | '\'' | 'n' | 't' | 'r'))) '\''
                { TOK_Char (lexeme lexbuf).[1] }
      | [' ' '\t']
                { token lexbuf }
      | '\n'    { incr_lineno lexbuf; token lexbuf }
      | eof     { TOK_EOF }
