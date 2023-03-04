(** [src_span] is a region in some source file.
    Used to report error messages *)
type src_span =
    { span_start : Lexing.position
    ; span_end   : Lexing.position }

(* Tokens are string literals in the input, while labels are the name of a
   collection of tokens. For example, ";" is a token while type is a label that
   could represent any type *)
type error_elem =
    | Token of string
    | Label of string
    | EndOfInput

let pp_error_elem fmt elem = match elem with
    | Token str  -> Format.fprintf fmt "\"%s\"" (String.escaped str)
    | Label str  -> Format.fprintf fmt "%s" str
    | EndOfInput -> Format.fprintf fmt "[end of input]"

type error =
    | Basic      of { unexpected : error_elem option
                    ; expecting  : error_elem list
                    ; message    : string option }
    | Unclosed   of string
    | Expecting  of string
    | Unexpected of string
    | BadToken   of string

exception Error of src_span * error

let pp_span fmt span =
    Format.fprintf fmt "[file \"%s\", row %d, col %d to row %d, col %d]"
        span.span_start.pos_fname
        span.span_start.pos_lnum
        (span.span_start.pos_cnum - span.span_start.pos_bol)
        span.span_end.pos_lnum  
        (span.span_end.pos_cnum - span.span_end.pos_bol)


let rec expecting_helper fmt exp_list =
    let open Format in
    match exp_list with
    | []      -> ();
    | e :: [] -> fprintf fmt " or "; pp_error_elem fmt e;
    | e :: es -> fprintf fmt ", "; pp_error_elem fmt e; expecting_helper fmt es


let pp_error fmt err =
    let open Format in
    let open Option in
    match err with
    | Basic {unexpected; expecting; message}
        ->  (if is_some unexpected then
                fprintf fmt "Unexpected ";
                match unexpected with
                | None       -> fprintf fmt ""
                | Some token -> pp_error_elem fmt token;
                fprintf fmt ".\n");
            (match expecting with
            | [] -> ();
            | _  -> fprintf fmt "Expecting ";
                pp_error_elem fmt (List.hd expecting);
                expecting_helper fmt (List.tl expecting);
                fprintf fmt ".\n");
            (match message with
            | None   -> fprintf fmt ""
            | Some m -> fprintf fmt "%s" m);
            fprintf fmt "\n"
    | Unclosed   msg -> fprintf fmt "Unclosed %s"   msg
    | Expecting  msg -> fprintf fmt "Expecting %s"  msg
    | Unexpected msg -> fprintf fmt "Unexpected %s" msg
    | BadToken   msg -> fprintf fmt "Bad token %s"  msg
