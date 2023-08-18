module OrderedString = struct
  type t = string
  let compare = Stdlib.compare
end

module StringMap = Map.Make(OrderedString)

type func_impl = Syntax.ParseTree.func_impl
type func_arg = Syntax.ParseTree.func_arg

let trans_func_impl (func_impl:func_impl): Semantics.ANF.function_definition =
  let ref_map = ref StringMap.empty in
  let get_fresh_arg_name (func_arg:func_arg) =
    let new_var = Semantics.ANF.gen_var() in
    ref_map := StringMap.add func_arg.farg_name new_var !ref_map;
    new_var in
  let (func_decl, stmt) = func_impl in
    {
      func_src = stmt.span;
      func_name = func_decl.func_decl_name;
      func_params = List.map get_fresh_arg_name func_decl.func_decl_args;
      func_label = 114514; (* 暂时不懂 *)
      func_body = Return(stmt.span, Val(Int(2)));
    }

let _ = Format.printf "1919810\n";
