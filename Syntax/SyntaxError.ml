
(** [src_span] is a region in some source file.
    Used to report error messages *)
type src_span =
    { span_start : Lexing.position
    ; span_end   : Lexing.position }

type error =
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


let pp_error fmt err =
    let open Format in
    match err with
    | Unclosed   msg -> fprintf fmt "unclosed %s"   msg
    | Expecting  msg -> fprintf fmt "expecting %s"  msg
    | Unexpected msg -> fprintf fmt "unexpected %s" msg
    | BadToken   msg -> fprintf fmt "bad token %s"  msg
