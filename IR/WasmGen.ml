(** WASM/WAT code generation from ANF IR.
    Generates WebAssembly Text Format (.wat). *)

open ANF

type wasm_module =
  { funcs     : wasm_func list
  ; memories  : int  (* number of memory pages *)
  ; globals   : string list
  }

and wasm_func =
  { wf_name   : string
  ; wf_params : (string * wasm_type) list
  ; wf_result : wasm_type option
  ; wf_locals : (string * wasm_type) list
  ; wf_body   : wasm_instr list
  }

and wasm_type = I32 | I64 | F32 | F64

and wasm_instr =
  | WLocalGet  of string
  | WLocalSet  of string
  | WLocalTee  of string
  | WGlobalGet of string
  | WGlobalSet of string
  | WI32Const  of int
  | WI64Const  of int64
  | WF32Const  of float
  | WF64Const  of float
  | WCall      of string
  | WCallIndirect of string  (* type signature *)
  | WBlock     of string option * wasm_instr list
  | WLoop      of string option * wasm_instr list
  | WIf        of string option * wasm_type option * wasm_instr list * wasm_instr list
  | WBr        of string
  | WBrIf      of string
  | WBrTable   of string list * string
  | WReturn
  | WDrop
  | WUnreachable
  | WNop
  | WI32Add | WI32Sub | WI32Mul | WI32Div_s | WI32Rem_s
  | WI32And | WI32Or  | WI32Xor | WI32Shl | WI32Shr_s
  | WI32Eq | WI32Ne | WI32Lt_s | WI32Le_s | WI32Gt_s | WI32Ge_s
  | WI32Wrap_i64 | WI32Extend_i64_s
  | WI64Add | WI64Sub | WI64Mul | WI64Div_s | WI64Rem_s
  | WI64And | WI64Or  | WI64Xor | WI64Shl | WI64Shr_s
  | WI64Eq | WI64Ne | WI64Lt_s | WI64Le_s | WI64Gt_s | WI64Ge_s
  | WF32Add | WF32Sub | WF32Mul | WF32Div
  | WF32Eq | WF32Ne | WF32Lt | WF32Le | WF32Gt | WF32Ge
  | WF64Add | WF64Sub | WF64Mul | WF64Div
  | WF64Eq | WF64Ne | WF64Lt | WF64Le | WF64Gt | WF64Ge
  | I32Load  | I32Store | I32Load8_s | I32Store8
  | I64Load  | I64Store
  | F32Load  | F32Store
  | F64Load  | F64Store
  | MemorySize | MemoryGrow
  | Select
  | Comment of string
[@@deriving show]

(** {1 WAT Pretty Printer} *)

let pp_wasm_type fmt = function
  | I32 -> Format.fprintf fmt "i32"
  | I64 -> Format.fprintf fmt "i64"
  | F32 -> Format.fprintf fmt "f32"
  | F64 -> Format.fprintf fmt "f64"

let rec pp_wasm_instr fmt = function
  | WLocalGet name  -> Format.fprintf fmt "local.get $%s" name
  | WLocalSet name  -> Format.fprintf fmt "local.set $%s" name
  | WLocalTee name  -> Format.fprintf fmt "local.tee $%s" name
  | WGlobalGet name -> Format.fprintf fmt "global.get $%s" name
  | WGlobalSet name -> Format.fprintf fmt "global.set $%s" name
  | WI32Const n     -> Format.fprintf fmt "i32.const %d" n
  | WI64Const n     -> Format.fprintf fmt "i64.const %Ld" n
  | WF32Const f     -> Format.fprintf fmt "f32.const %F" f
  | WF64Const f     -> Format.fprintf fmt "f64.const %F" f
  | WCall name      -> Format.fprintf fmt "call $%s" name
  | WCallIndirect ty -> Format.fprintf fmt "call_indirect (type $%s)" ty
  | WBlock (label, instrs) ->
      let lbl = match label with Some l -> Format.sprintf " $%s" l | None -> "" in
      Format.fprintf fmt "@[<v2>(block%s@ %a)@]" lbl pp_instrs instrs
  | WLoop (label, instrs) ->
      let lbl = match label with Some l -> Format.sprintf " $%s" l | None -> "" in
      Format.fprintf fmt "@[<v2>(loop%s@ %a)@]" lbl pp_instrs instrs
  | WIf (label, result, then_instrs, else_instrs) ->
      let lbl = match label with Some l -> Format.sprintf " $%s" l | None -> "" in
      let res = match result with
        | Some ty ->
            let ty_str = match ty with
              | I32 -> "i32" | I64 -> "i64" | F32 -> "f32" | F64 -> "f64" in
            Format.sprintf " (result %s)" ty_str
        | None -> "" in
      Format.fprintf fmt "@[<v2>(if%s%s@ @[<v2>(then@ %a)@]@ @[<v2>(else@ %a)@])@]"
        lbl res pp_instrs then_instrs pp_instrs else_instrs
  | WBr label -> Format.fprintf fmt "br $%s" label
  | WBrIf label -> Format.fprintf fmt "br_if $%s" label
  | WBrTable (labels, default) ->
      Format.fprintf fmt "br_table %a $%s"
        (Format.pp_print_list (fun fmt l -> Format.fprintf fmt "$%s " l)) labels default
  | WReturn -> Format.fprintf fmt "return"
  | WDrop -> Format.fprintf fmt "drop"
  | WUnreachable -> Format.fprintf fmt "unreachable"
  | WNop -> Format.fprintf fmt "nop"
  | WI32Add -> Format.fprintf fmt "i32.add"
  | WI32Sub -> Format.fprintf fmt "i32.sub"
  | WI32Mul -> Format.fprintf fmt "i32.mul"
  | WI32Div_s -> Format.fprintf fmt "i32.div_s"
  | WI32Rem_s -> Format.fprintf fmt "i32.rem_s"
  | WI32And -> Format.fprintf fmt "i32.and"
  | WI32Or  -> Format.fprintf fmt "i32.or"
  | WI32Xor -> Format.fprintf fmt "i32.xor"
  | WI32Shl -> Format.fprintf fmt "i32.shl"
  | WI32Shr_s -> Format.fprintf fmt "i32.shr_s"
  | WI32Eq -> Format.fprintf fmt "i32.eq"
  | WI32Ne -> Format.fprintf fmt "i32.ne"
  | WI32Lt_s -> Format.fprintf fmt "i32.lt_s"
  | WI32Le_s -> Format.fprintf fmt "i32.le_s"
  | WI32Gt_s -> Format.fprintf fmt "i32.gt_s"
  | WI32Ge_s -> Format.fprintf fmt "i32.ge_s"
  | WI32Wrap_i64 -> Format.fprintf fmt "i32.wrap_i64"
  | WI64Add -> Format.fprintf fmt "i64.add"
  | WI64Sub -> Format.fprintf fmt "i64.sub"
  | WI64Mul -> Format.fprintf fmt "i64.mul"
  | WI64Div_s -> Format.fprintf fmt "i64.div_s"
  | WI64Rem_s -> Format.fprintf fmt "i64.rem_s"
  | WI64And -> Format.fprintf fmt "i64.and"
  | WI64Or  -> Format.fprintf fmt "i64.or"
  | WI64Xor -> Format.fprintf fmt "i64.xor"
  | WI64Shl -> Format.fprintf fmt "i64.shl"
  | WI64Shr_s -> Format.fprintf fmt "i64.shr_s"
  | WI64Eq -> Format.fprintf fmt "i64.eq"
  | WI64Ne -> Format.fprintf fmt "i64.ne"
  | WI64Lt_s -> Format.fprintf fmt "i64.lt_s"
  | WI64Le_s -> Format.fprintf fmt "i64.le_s"
  | WI64Gt_s -> Format.fprintf fmt "i64.gt_s"
  | WI64Ge_s -> Format.fprintf fmt "i64.ge_s"
  | WF32Add -> Format.fprintf fmt "f32.add"
  | WF32Sub -> Format.fprintf fmt "f32.sub"
  | WF32Mul -> Format.fprintf fmt "f32.mul"
  | WF32Div -> Format.fprintf fmt "f32.div"
  | WF32Eq -> Format.fprintf fmt "f32.eq"
  | WF32Ne -> Format.fprintf fmt "f32.ne"
  | WF32Lt -> Format.fprintf fmt "f32.lt"
  | WF32Le -> Format.fprintf fmt "f32.le"
  | WF32Gt -> Format.fprintf fmt "f32.gt"
  | WF32Ge -> Format.fprintf fmt "f32.ge"
  | WF64Add -> Format.fprintf fmt "f64.add"
  | WF64Sub -> Format.fprintf fmt "f64.sub"
  | WF64Mul -> Format.fprintf fmt "f64.mul"
  | WF64Div -> Format.fprintf fmt "f64.div"
  | WF64Eq -> Format.fprintf fmt "f64.eq"
  | WF64Ne -> Format.fprintf fmt "f64.ne"
  | WF64Lt -> Format.fprintf fmt "f64.lt"
  | WF64Le -> Format.fprintf fmt "f64.le"
  | WF64Gt -> Format.fprintf fmt "f64.gt"
  | WF64Ge -> Format.fprintf fmt "f64.ge"
  | I32Load      -> Format.fprintf fmt "i32.load"
  | I32Store     -> Format.fprintf fmt "i32.store"
  | I32Load8_s   -> Format.fprintf fmt "i32.load8_s"
  | I32Store8    -> Format.fprintf fmt "i32.store8"
  | I64Load      -> Format.fprintf fmt "i64.load"
  | I64Store     -> Format.fprintf fmt "i64.store"
  | F32Load      -> Format.fprintf fmt "f32.load"
  | F32Store     -> Format.fprintf fmt "f32.store"
  | F64Load      -> Format.fprintf fmt "f64.load"
  | F64Store     -> Format.fprintf fmt "f64.store"
  | MemorySize   -> Format.fprintf fmt "memory.size"
  | MemoryGrow   -> Format.fprintf fmt "memory.grow"
  | Select       -> Format.fprintf fmt "select"
  | Comment msg  -> Format.fprintf fmt ";; %s" msg
  | WI32Extend_i64_s -> Format.fprintf fmt "i64.extend_i32_s"

and pp_instrs fmt instrs =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list pp_wasm_instr) instrs

let pp_wasm_func fmt { wf_name; wf_params; wf_result; wf_locals; wf_body } =
  let pp_param fmt (name, ty) = Format.fprintf fmt "(param $%s %a)" name pp_wasm_type ty in
  let pp_local fmt (name, ty) = Format.fprintf fmt "(local $%s %a)" name pp_wasm_type ty in
  let result = match wf_result with
    | Some ty -> Format.asprintf "(result %a)" pp_wasm_type ty
    | None -> ""
  in
  Format.fprintf fmt "@[<v2>(func $%s@ %a@ %s@ %a@ @[<v2>%a@])@]@,"
    wf_name
    (Format.pp_print_list pp_param) wf_params
    result
    (Format.pp_print_list pp_local) wf_locals
    pp_instrs wf_body

let pp_wasm_module fmt { funcs; memories; globals } =
  Format.fprintf fmt "@[<v>(module@,";
  (if memories > 0 then
    Format.fprintf fmt "  (memory (export \"memory\") %d)@," memories);
  List.iter (fun g -> Format.fprintf fmt "  (global $%s (mut i32) (i32.const 1024))@," g) globals;
  List.iter (Format.fprintf fmt "  %a@," pp_wasm_func) funcs;
  Format.fprintf fmt ")@]@."

(** {1 Bump Allocator Helpers} *)

let bump_allocator_funcs : wasm_func list =
  [ { wf_name = "bump_init"
    ; wf_params = ["size", I32]
    ; wf_result = None
    ; wf_locals = []
    ; wf_body = [ Comment "Initialize heap pointer";
                 WLocalGet "size"; WGlobalSet "heap_ptr" ]
    }
  ; { wf_name = "bump_alloc"
    ; wf_params = ["size", I32]
    ; wf_result = Some I32
    ; wf_locals = ["ptr", I32]
    ; wf_body = [ Comment "Simple bump allocator: return old heap pointer, advance by size";
                 WGlobalGet "heap_ptr"; WLocalSet "ptr";
                 WGlobalGet "heap_ptr"; WLocalGet "size"; WI32Add; WGlobalSet "heap_ptr";
                 WLocalGet "ptr" (* return old ptr *) ]
    }
  ; { wf_name = "bump_alloc_zero"
    ; wf_params = ["size", I32]
    ; wf_result = Some I32
    ; wf_locals = ["ptr", I32; "i", I32]
    ; wf_body = [ WLocalGet "size"; WCall "bump_alloc"; WLocalSet "ptr";
                 (* Zero out the memory: loop over bytes *)
                 WI32Const 0; WLocalSet "i";
                 WBlock (Some "zero_end", [
                   WLoop (Some "zero_loop", [
                     WLocalGet "i"; WLocalGet "size"; WI32Ge_s;
                     WIf (None, None, [WBr "zero_end"], [WNop]);
                     WLocalGet "ptr"; WLocalGet "i"; WI32Add; WI32Const 0; I32Store8;
                     WLocalGet "i"; WI32Const 1; WI32Add; WLocalSet "i";
                     WBr "zero_loop"
                   ])
                 ]);
                 WLocalGet "ptr" ]
    }
  ]

(** {1 ANF to WASM Compilation} *)

(** Compilation context: maps ANF variables to WASM local names *)
type label_kind =
  | LoopLabel                        (* jump → br back to loop head (continue) *)
  | ExitLabel                        (* jump → br out of a loop's exit block (break) *)
  | BlockLabel of ANF.block_definition (* jump → inline the continuation body *)

type compile_ctx =
  { blocks      : (label, string) Hashtbl.t  (* ANF label → WASM block name *)
  ; label_kinds : (label, label_kind) Hashtbl.t
  ; vars        : (variable, string) Hashtbl.t  (* ANF variable → WASM local name *)
  ; var_types   : (variable, wasm_type) Hashtbl.t (* ANF variable → WASM type *)
  ; func_consts : (variable, string) Hashtbl.t (* ANF variable → known function name *)
  ; funcs       : (func_name, string) Hashtbl.t (* func name *)
  ; heap_ptr    : int ref
  ; tmp_counter : int ref
  }

let mk_compile_ctx () =
  { blocks = Hashtbl.create 16
  ; label_kinds = Hashtbl.create 16
  ; vars   = Hashtbl.create 64
  ; var_types = Hashtbl.create 64
  ; func_consts = Hashtbl.create 16
  ; funcs  = Hashtbl.create 16
  ; heap_ptr = ref 0
  ; tmp_counter = ref 0
  }

let var_wasm_name ctx v =
  match Hashtbl.find_opt ctx.vars v with
  | Some name -> name
  | None ->
      let name = Printf.sprintf "v%d" v in
      Hashtbl.add ctx.vars v name;
      name

let set_var_type ctx v ty = Hashtbl.replace ctx.var_types v ty

let get_var_type ctx v =
  match Hashtbl.find_opt ctx.var_types v with
  | Some ty -> ty
  | None -> I32

let label_wasm_name ctx l =
  match Hashtbl.find_opt ctx.blocks l with
  | Some name -> name
  | None ->
      let name = Printf.sprintf "l%d" l in
      Hashtbl.add ctx.blocks l name;
      name

let fresh_tmp ctx =
  let n = !(ctx.tmp_counter) in
  incr ctx.tmp_counter;
  Printf.sprintf "t%d" n

(** Memory layout helpers.
    All values are stored as i32 (pointers or immediate values).
    Bool: 0 or 1 (stored as i32)
    Int: i32
    Char: i32 (16-bit value)
    Float: f32/f64 stored as immediate
    Struct: pointer to heap-allocated struct
    ADT: pointer to heap-allocated ADT (tag word + payload)
    Tuple: pointer to heap-allocated tuple
*)

(** How many bytes a type takes in memory (for heap allocation) *)
let rec size_of_type (table : Semantics.Table.table) (ty : Syntax.ParseTree.typ) : int =
  match ty.shape with
  | TyUnit | TyBool | TyInt _ | TyChar -> 4       (* i32 *)
  | TyFloat FSize_32 -> 4
  | TyFloat FSize_64 -> 8
  | TyThis -> 4  (* pointer *)
  | TyFunc _ -> 4 (* function pointer *)
  | TyVar _ -> 4
  | TyArray (elem_ty, len) -> size_of_type table elem_ty * len
  | TyTuple tys -> List.fold_left (fun acc ty -> acc + size_of_type table ty) 0 tys
  | TyNamed (name, _) ->
      match Hashtbl.find_opt table.typ name with
      | Some (Struct_data data) ->
          List.fold_left (fun acc (_, ty, _) -> acc + size_of_type table ty) 0 data.fields
      | Some (ADT_data data) ->
          4 (* tag *) +
          List.fold_left (fun acc (_, tys) ->
            acc + List.fold_left (fun a ty -> a + size_of_type table ty) 0 tys) 0 data.core
      | _ -> 4

(** Field offset for a named struct field *)
let field_offset_of_struct (table : Semantics.Table.table) (struct_name : string) (field_name : string) : int =
  match Hashtbl.find_opt table.typ struct_name with
  | Some (Struct_data data) ->
      let rec find_offset offset = function
        | [] -> failwith (Printf.sprintf "field %s not found in struct %s" field_name struct_name)
        | (fname, fty, _) :: rest ->
            if fname = field_name then offset
            else find_offset (offset + size_of_type table fty) rest
      in
      find_offset 0 data.fields
  | _ -> failwith (Printf.sprintf "%s is not a struct type" struct_name)

(** Compile expression value: evaluate to a WASM value.
    Returns (instrs_to_compute_value, wasm_type_of_result) *)
let rec comp_value (ctx : compile_ctx) (table : Semantics.Table.table) (value : ANF.value) : wasm_instr list * wasm_type =
  match value with
  | Int n -> ([WI32Const n], I32)
  | Float f -> ([WF32Const f], F32)
  | String _s -> ([WI32Const 0], I32) (* Strings not fully supported yet *)
  | LVal { lv_var; lv_path = []; _ } ->
      let var_name = var_wasm_name ctx lv_var in
      ([WLocalGet var_name], get_var_type ctx lv_var)
  | LVal { lv_var; lv_path; _ } ->
      let var_name = var_wasm_name ctx lv_var in
      let base_instrs, base_ty = [WLocalGet var_name], get_var_type ctx lv_var in
      comp_path ctx table lv_path base_instrs base_ty

and comp_path (ctx : compile_ctx) (table : Semantics.Table.table) (path : path) (acc : wasm_instr list) (acc_ty : wasm_type) : wasm_instr list * wasm_type =
  match path with
  | [] -> (acc, acc_ty)
  | pn :: rest ->
      match pn with
      | Field n ->
          (* pointer + offset → load *)
          let offset = n * 4 in (* each field is i32-aligned for simplicity *)
          let new_instrs = acc @ [WI32Const offset; WI32Add; I32Load] in
          comp_path ctx table rest new_instrs I32
      | FieldByName fname ->
          (* Need to resolve field name to index - try looking up in all struct types *)
          let offset = resolve_field_offset table fname in
          let new_instrs = acc @ [Comment (Printf.sprintf "field %s at offset %d" fname offset);
                                  WI32Const offset; WI32Add; I32Load] in
          comp_path ctx table rest new_instrs I32
      | AsTag t ->
          (* Access the t-th field after the tag *)
          let offset = 4 + t * 4 in
          let new_instrs = acc @ [WI32Const offset; WI32Add; I32Load] in
          comp_path ctx table rest new_instrs I32
      | Tag ->
          (* Read the tag: first word at the pointer *)
          let new_instrs = acc @ [I32Load] in
          comp_path ctx table rest new_instrs I32
      | Method _m ->
          (* Method dispatch not yet fully implemented; return pointer as-is *)
          comp_path ctx table rest acc I32
      | Deref ->
          let new_instrs = acc @ [I32Load] in
          comp_path ctx table rest new_instrs I32

and resolve_field_offset (table : Semantics.Table.table) (fname : string) : int =
  (* Scan all struct types to find this field name's offset *)
  Hashtbl.fold (fun _tname tdata acc ->
    match acc with
    | Some _ -> acc
    | None ->
        match tdata with
        | Semantics.Table.Struct_data data ->
            let rec find offset = function
              | [] -> None
              | (fn, fty, _) :: rest ->
                  if fn = fname then Some offset
                  else find (offset + size_of_type table fty) rest
            in
            find 0 data.fields
        | _ -> None
  ) table.typ None
  |> function
  | Some offset -> offset
  | None -> failwith (Printf.sprintf "Cannot resolve field %s: not found in any struct" fname)

(** Compile an ANF expression to WASM instructions that leave the result on the stack *)
let rec comp_expr (ctx : compile_ctx) (table : Semantics.Table.table) (expr : ANF.expr) : wasm_instr list * wasm_type =
  match expr with
  | Val v -> comp_value ctx table v
  | Copy lv ->
      (* Load value from lvalue *)
      comp_value ctx table (LVal lv)
  | Borrow (_, lv) ->
      (* Borrow is just a pointer to the lvalue *)
      let var_name = var_wasm_name ctx lv.lv_var in
      ([WLocalGet var_name], I32)
  | App (func_val, args) ->
      let arg_instrs, _ = comp_values ctx table args in
      let call =
        match func_val with
        | LVal { lv_var; lv_path = []; _ } ->
            (match Hashtbl.find_opt ctx.func_consts lv_var with
             | Some fname -> [WCall fname]
             | None -> [WUnreachable] (* indirect/method call not yet supported *))
        | _ -> [WUnreachable] (* indirect/method call not yet supported *)
      in
      (arg_instrs @ call, I32) (* Assume all return i32 for now *)
  | UnOp (op, v) ->
      let (vinstrs, vty) = comp_value ctx table v in
      let wasm_op = match op with
        | Syntax.ParseTree.UnOpNeg ->
            begin match vty with
            | I32 -> WI32Mul (* negate: mul by -1 *)
            | F32 -> WF32Mul
            | _ -> failwith "unsupported unary negate type"
            end
        | Syntax.ParseTree.UnOpNot ->
            (* logical not: eqz *)
            WI32Eq (* check eq with 0 *)
        | Syntax.ParseTree.UnOpPreInc | Syntax.ParseTree.UnOpPreDec ->
            failwith "Pre-inc/dec should be decomposed in conversion, not reachable here"
      in
      begin match op with
      | UnOpNeg ->
          let neg_one = match vty with I32 -> WI32Const (-1) | F32 -> WF32Const (-1.0) | _ -> failwith "" in
          (vinstrs @ [neg_one; wasm_op], vty)
      | UnOpNot ->
          (vinstrs @ [WI32Const 0; wasm_op], I32)
      | UnOpPreInc | UnOpPreDec ->
          failwith "Pre-inc/dec should be decomposed in conversion, not reachable here"
      end
  | BinOp (op, lhs, rhs) ->
      let (linstrs, lty) = comp_value ctx table lhs in
      let (rinstrs, _rty) = comp_value ctx table rhs in
      let wasm_op = comp_binop op lty in
      (linstrs @ rinstrs @ [wasm_op], I32) (* Assume i32 result *)
  | MkData (kind, values) ->
      comp_mkdata ctx table kind values
  | Fun _fname -> ([WI32Const 0 (* function reference *)], I32)

and comp_values (ctx : compile_ctx) (table : Semantics.Table.table) (values : ANF.value list) : wasm_instr list * wasm_type list =
  let results = List.map (fun v -> comp_value ctx table v) values in
  (List.concat_map fst results, List.map snd results)

and comp_binop (op : Syntax.ParseTree.binary_op) (_ty : wasm_type) : wasm_instr =
  match op with
  | BinOpCompare cmp -> begin match cmp with
      | BinOpLt  -> WI32Lt_s  | BinOpLeq -> WI32Le_s
      | BinOpGt  -> WI32Gt_s  | BinOpGeq -> WI32Ge_s
      | BinOpEq  -> WI32Eq    | BinOpNeq -> WI32Ne
      end
  | BinOpCalculate calc -> begin match calc with
      | BinOpAdd -> WI32Add    | BinOpSub -> WI32Sub
      | BinOpMul -> WI32Mul    | BinOpDiv -> WI32Div_s
      | BinOpMod -> WI32Rem_s
      | BinOpLOr  -> WI32Or    | BinOpLAnd -> WI32And
      | BinOpLXor -> WI32Xor
      | BinOpBOr  -> WI32Or    | BinOpBAnd -> WI32And
      | BinOpBXor -> WI32Xor
      | BinOpLShift -> WI32Shl | BinOpRShift -> WI32Shr_s
      end

and comp_mkdata (ctx : compile_ctx) (table : Semantics.Table.table) (kind : data_kind) (values : value list) : wasm_instr list * wasm_type =
  let val_pairs = List.map (fun v -> comp_value ctx table v) values in
  let total_size = match kind with
    | Tuple n -> n * 4
    | Struct name ->
        begin match Hashtbl.find_opt table.typ name with
        | Some (Struct_data data) ->
            List.fold_left (fun acc (_, ty, _) -> acc + size_of_type table ty) 0 data.fields
        | _ -> failwith (Printf.sprintf "struct %s not found" name)
        end
    | ADT (_, _) -> 4 + List.length values * 4
    | Impl _ -> 4 (* vtable pointer *)
  in
  (* [items] = (byte offset, value instructions to store) pairs, in store order. *)
  let items : (int * wasm_instr list) list =
    match kind with
    | Tuple _ ->
        List.mapi (fun i (vi, _) -> (i * 4, vi)) val_pairs
    | Struct name ->
        begin match Hashtbl.find_opt table.typ name with
        | Some (Struct_data data) ->
            let rec offsets acc = function
              | [] -> []
              | (_, ty, _) :: rest -> acc :: offsets (acc + size_of_type table ty) rest
            in
            List.map2 (fun (vi, _) off -> (off, vi)) val_pairs (offsets 0 data.fields)
        | _ -> failwith (Printf.sprintf "struct %s not found" name)
        end
    | ADT (_, label) ->
        let tag =
          match Hashtbl.find_opt table.adt label with
          | Some d -> d.tag
          | None -> 0
        in
        (0, [WI32Const tag]) :: List.mapi (fun i (vi, _) -> (4 + i * 4, vi)) val_pairs
    | Impl _ ->
        [(0, [WI32Const 0])]
  in
  let ptr_tmp = fresh_tmp ctx in
  let alloc_instrs = [WI32Const total_size; WCall "bump_alloc"; WLocalSet ptr_tmp] in
  let store_instrs =
    List.concat (List.map (fun (off, vi) ->
      [WLocalGet ptr_tmp; WI32Const off] @ vi @ [I32Store]) items)
  in
  (alloc_instrs @ store_instrs @ [WLocalGet ptr_tmp], I32)

(** Compile an ANF program segment to WASM instructions *)
let rec comp_program (ctx : compile_ctx) (table : Semantics.Table.table) (prog : ANF.program) : wasm_instr list =
  match prog with
  | Empty -> [WNop]
  | Abort -> [WUnreachable]
  | Jump (_, label, values) ->
      comp_jump ctx table label values
  | Stmt (_, stmt, rest) ->
      let stmt_instrs = comp_statement ctx table stmt in
      stmt_instrs @ comp_program ctx table rest
  | Branch br ->
      comp_branch ctx table br
  | Block (def, rest) ->
      (* A linear continuation: [rest] runs first and tail-calls [def] via Jump.
         We register [def] as a BlockLabel so each Jump site inlines its body,
         and here we only compile [rest]. *)
      Hashtbl.replace ctx.label_kinds def.blk_label (BlockLabel def);
      comp_program ctx table rest
  | Loop def ->
      (* A loop in this CPS is shaped as [Loop { Block #exit = after in rest }]:
         [rest] is the loop condition/body, which either [jump #loop] (continue)
         or [jump #exit] (break).  We turn the exit continuation into a block
         label, wrap the loop inside it, and place [after] after the loop. *)
      (match def.blk_body with
       | Block (exit_def, rest) ->
           Hashtbl.replace ctx.label_kinds def.blk_label LoopLabel;
           Hashtbl.replace ctx.label_kinds exit_def.blk_label ExitLabel;
           let loop_name = label_wasm_name ctx def.blk_label in
           let exit_name = label_wasm_name ctx exit_def.blk_label in
           let rest_instrs = comp_program ctx table rest in
           let after_instrs = comp_program ctx table exit_def.blk_body in
           [ WBlock (Some exit_name, [ WLoop (Some loop_name, rest_instrs) ]) ]
           @ after_instrs
       | _ ->
           Hashtbl.replace ctx.label_kinds def.blk_label LoopLabel;
           let loop_name = label_wasm_name ctx def.blk_label in
           let loop_body = comp_program ctx table def.blk_body in
           [ WLoop (Some loop_name, loop_body) ])

and comp_jump (ctx : compile_ctx) (table : Semantics.Table.table) (label : label) (values : value list) : wasm_instr list =
  let lbl = label_wasm_name ctx label in
  match Hashtbl.find_opt ctx.label_kinds label with
  | Some LoopLabel | Some ExitLabel ->
      (* continue / break: branch to the loop head or out of the loop's exit block *)
      let val_instrs = List.concat_map (fun v -> fst (comp_value ctx table v)) values in
      val_instrs @ [WBr lbl]
  | Some (BlockLabel def) ->
      (* call a linear continuation: bind its parameters, then inline its body *)
      let bind_instrs = comp_bind_params ctx table def.blk_params values in
      bind_instrs @ comp_program ctx table def.blk_body
  | None ->
      (* function return label: return the value (unit → 0) *)
      let val_instrs = List.concat_map (fun v -> fst (comp_value ctx table v)) values in
      let val_instrs = if values = [] then [WI32Const 0] else val_instrs in
      val_instrs @ [WReturn]

and comp_bind_params (ctx : compile_ctx) (table : Semantics.Table.table) (params : variable list) (values : value list) : wasm_instr list =
  let rec go ps vs =
    match ps, vs with
    | [], _ | _, [] -> []
    | p :: ps', v :: vs' ->
        let pname = var_wasm_name ctx p in
        let (vinstrs, vty) = comp_value ctx table v in
        set_var_type ctx p vty;
        vinstrs @ [WLocalSet pname] @ go ps' vs'
  in
  go params values

and comp_branch (ctx : compile_ctx) (table : Semantics.Table.table) (br : ANF.branching) : wasm_instr list =
  let matched_instrs, _ = comp_value ctx table br.br_matched in
  let rec compile_cases = function
    | [] ->
        (match br.br_default with
         | Some p -> comp_program ctx table p
         | None -> [WUnreachable])
    | (tag, prog) :: rest ->
        let case_instrs = comp_program ctx table prog in
        let rest_instrs = compile_cases rest in
        matched_instrs @ [WI32Const tag; WI32Eq; WIf (None, None, case_instrs, rest_instrs)]
  in
  compile_cases br.br_branches

and comp_statement (ctx : compile_ctx) (table : Semantics.Table.table) (stmt : ANF.statement) : wasm_instr list =
  match stmt with
  | Decl (var, expr) ->
      let var_name = var_wasm_name ctx var in
      let (expr_instrs, expr_ty) = comp_expr ctx table expr in
      set_var_type ctx var expr_ty;
      (match expr with
       | ANF.Fun fname -> Hashtbl.replace ctx.func_consts var fname
       | _ -> ());
      expr_instrs @ [WLocalSet var_name]
  | Assign (lv, value) ->
      let (val_instrs, _) = comp_value ctx table value in
      let (addr_instrs, _) = comp_value ctx table (LVal lv) in
      addr_instrs @ val_instrs @ [I32Store]
  | EndScope _vars -> [] (* Scope management not needed in WASM *)

(** Compile a function definition to WASM *)
let comp_func (ctx : compile_ctx) (table : Semantics.Table.table) (func_def : ANF.function_definition) : wasm_func =
  Hashtbl.clear ctx.vars;
  Hashtbl.clear ctx.blocks;
  Hashtbl.clear ctx.var_types;
  Hashtbl.clear ctx.func_consts;
  Hashtbl.clear ctx.label_kinds;
  ctx.tmp_counter := 0;

  let param_names = List.map (fun v -> var_wasm_name ctx v) func_def.func_params in
  List.iter (fun v -> set_var_type ctx v I32) func_def.func_params;
  let params = List.map (fun n -> (n, I32)) param_names in

  let body = comp_program ctx table func_def.func_body in

  (* Collect non-parameter locals, ordered by their ANF variable number. *)
  let locals =
    Hashtbl.fold (fun v name acc ->
      if List.mem v func_def.func_params then acc
      else (v, name, get_var_type ctx v) :: acc)
      ctx.vars []
    |> List.sort (fun (v1, _, _) (v2, _, _) -> compare v1 v2)
    |> List.map (fun (_, name, ty) -> (name, ty))
  in
  (* Temporary locals used by aggregate construction. *)
  let tmp_locals =
    List.init !(ctx.tmp_counter) (fun i -> (Printf.sprintf "t%d" i, I32))
  in

  { wf_name = func_def.func_name
  ; wf_params = params
  ; wf_result = Some I32
  ; wf_locals = locals @ tmp_locals
  ; wf_body = body @ [WUnreachable]
  }

(** {1 FieldByName → Field(index) resolution pass}

    Before code generation, we resolve all [FieldByName] occurrences in the ANF
    to concrete [Field] indices by scanning the type table. This is a global scan
    approach: field names are assumed to be unique across all struct types.
    TODO: carry type information in ANF for precise per-type field resolution. *)

let build_field_map (table : Semantics.Table.table) : (string, int) Hashtbl.t =
  let field_map = Hashtbl.create 16 in
  Hashtbl.iter (fun _tname tdata ->
    match tdata with
    | Semantics.Table.Struct_data data ->
        let rec add_fields offset = function
          | [] -> ()
          | (fname, fty, _) :: rest ->
              if not (Hashtbl.mem field_map fname) then
                Hashtbl.add field_map fname offset;
              add_fields (offset + size_of_type table fty) rest
        in
        add_fields 0 data.fields
    | _ -> ())
    table.typ;
  field_map

let resolve_field_name (field_map : (string, int) Hashtbl.t) (fname : string) : int =
  match Hashtbl.find_opt field_map fname with
  | Some offset -> offset
  | None -> failwith (Printf.sprintf "Cannot resolve field %s: not found in any struct" fname)

let rec resolve_path_node (field_map : (string, int) Hashtbl.t) (pn : ANF.path_node) : ANF.path_node =
  match pn with
  | ANF.FieldByName fname -> ANF.Field (resolve_field_name field_map fname)
  | _ -> pn

and resolve_lvalue (field_map : (string, int) Hashtbl.t) (lv : ANF.lvalue) : ANF.lvalue =
  { lv with lv_path = List.map (resolve_path_node field_map) lv.lv_path }

and resolve_value (field_map : (string, int) Hashtbl.t) (v : ANF.value) : ANF.value =
  match v with
  | ANF.LVal lv -> ANF.LVal (resolve_lvalue field_map lv)
  | _ -> v

and resolve_expr (field_map : (string, int) Hashtbl.t) (e : ANF.expr) : ANF.expr =
  match e with
  | ANF.Val v -> ANF.Val (resolve_value field_map v)
  | ANF.Copy lv -> ANF.Copy (resolve_lvalue field_map lv)
  | ANF.Borrow (b, lv) -> ANF.Borrow (b, resolve_lvalue field_map lv)
  | ANF.UnOp (op, v) -> ANF.UnOp (op, resolve_value field_map v)
  | ANF.BinOp (op, v1, v2) -> ANF.BinOp (op, resolve_value field_map v1, resolve_value field_map v2)
  | ANF.App (v, vs) -> ANF.App (resolve_value field_map v, List.map (resolve_value field_map) vs)
  | ANF.MkData (kind, vs) -> ANF.MkData (kind, List.map (resolve_value field_map) vs)
  | ANF.Fun _ -> e

and resolve_program (field_map : (string, int) Hashtbl.t) (prog : ANF.program) : ANF.program =
  match prog with
  | ANF.Empty -> ANF.Empty
  | ANF.Abort -> ANF.Abort
  | ANF.Jump (span, label, values) ->
      ANF.Jump (span, label, List.map (resolve_value field_map) values)
  | ANF.Stmt (span, stmt, rest) ->
      ANF.Stmt (span, resolve_statement field_map stmt, resolve_program field_map rest)
  | ANF.Branch br ->
      ANF.Branch { br with
        br_matched = resolve_value field_map br.br_matched;
        br_branches = List.map (fun (tag, p) -> (tag, resolve_program field_map p)) br.br_branches;
        br_default = Option.map (resolve_program field_map) br.br_default }
  | ANF.Block (def, rest) ->
      ANF.Block ({ def with blk_body = resolve_program field_map def.blk_body }, resolve_program field_map rest)
  | ANF.Loop def ->
      ANF.Loop { def with blk_body = resolve_program field_map def.blk_body }

and resolve_statement (field_map : (string, int) Hashtbl.t) (stmt : ANF.statement) : ANF.statement =
  match stmt with
  | ANF.Decl (var, e) -> ANF.Decl (var, resolve_expr field_map e)
  | ANF.Assign (lv, v) -> ANF.Assign (resolve_lvalue field_map lv, resolve_value field_map v)
  | ANF.EndScope vs -> ANF.EndScope vs

let resolve_field_names (table : Semantics.Table.table) (funcs : ANF.function_definition list)
    : ANF.function_definition list =
  let field_map = build_field_map table in
  List.map (fun fd ->
    { fd with func_body = resolve_program field_map fd.func_body })
    funcs

(** Compile an entire program (list of function definitions) to a WASM module *)
let comp_module (table : Semantics.Table.table) (funcs : ANF.function_definition list) : wasm_module =
  let ctx = mk_compile_ctx () in
  let resolved_funcs = resolve_field_names table funcs in
  let user_funcs = List.map (comp_func ctx table) resolved_funcs in
  { funcs = bump_allocator_funcs @ user_funcs
  ; memories = 1  (* one default memory page *)
  ; globals = ["heap_ptr"] (* bump allocator heap pointer *)
  }

(** Generate WAT text from ANF function definitions *)
let generate_wat (table : Semantics.Table.table) (funcs : ANF.function_definition list) : string =
  let wasm_mod = comp_module table funcs in
  Format.asprintf "%a" pp_wasm_module wasm_mod
