## Walk 1st, table, type

### Discuss, TODO and REMAIN list
1. Discuss BuiltinType.var.span = None
2. Discuss literal for int and float, ? i64? ? f64?
3. TODO method field 名字不重名检查 
4. TODO Test
5. REMAIN Pattern Exhaustivity
6. REMAIN ... subtyping for interface extension

### dev notes
#### 20220410
1. Decl这里需要修改为option的value么？还是强制要求赋初值？我个人倾向于强制赋初值。
```OCaml
type stmt_shape =
   (* ... *)
   | StmtDecl     of pattern * expr (* option ? *)
   (* ... *)
   ;
```

2. AST的struct_def_field，建议修改为
```OCaml
type is_delegated = 
   | IsDelegated
   | JustMember
and struct_def_field = 
   is_delegated * struct_field (* name *) * typ
   ;;
```
#### 20220319
1. TyExists
   ```OCaml
   { vpat_mut  : mutability
   ; vpat_typ  : typ option (* Only TyCk now, must be Some(ty) *)
   ; vpat_name : variable
   ; vpat_symb : symbol }
   ```

2. Delegate : modify search order DONE
3. BuiltinType.this is just a hole, DO NOT use
4. AST::func_decl 没有支持声明参数是某一接口，即
   ```OCaml
   type func_arg =
    { farg_name : variable
    ; farg_symb : symbol
    ; farg_typ  : 
    typ }
   type func_decl =
    { (* ... *)
      func_decl_args : func_arg list
    ; (* ... *)
   ```
   假设此处以TyVar(name)作为intf_name，那么也可以做，
   反正intf_name, type_name(adt, struct)都是不交的。
   将intf, type(adt, struct)放到一张表格里去吧，同一个命名空间
   Done

5. TODO method field 名字检查 
6. DONE function retv check
7. REMAIN ...
8. Discuss literal for int and float, ? i64? ? f64?
9. Discuss : return x:Intf reject
   ```deeplang
   fun foo(x:intf){ // -> reject
      return x;
   }
   ```

#### 20220305 
1. 变量名可以覆盖函数名（实现起来简单）
   ```OCaml
   let foo x = x+3;
   let bar x = let foo = 4 in x + foo; (* Hide function foo *)
   ```
2. walk_stmt 时要传入当前的函数名称，以便return expr时检查类型一致
3. modify ParseTree
   ```OCaml
   type methods_impl =
    { impl_intf : intf_name option
    ; impl_typ  : typ_name 
    (* just type name *)
    ; impl_id   : NodeId.impl
    ; impl_methods : func_impl list }
   ```
4. Discuss modify ParseTree : NodeId.func, NodeId.method
   ```OCaml
   type func_decl =
   { func_decl_name : func_name
   ; func_decl_args : func_arg list
   ; func_decl_id   : NodeId.func (* it seems not needed *)
   ; func_decl_ret  : typ }
   ```
   also in ```type methods_impl```
5. Question : table { 6 Hashtbl field } 是否可变？尚未测试
   Hashtbl ```utop```
6. 目前还没有检查成员函数与成员变量的重名情况，需要在```walk_method_decl```内修改
7. 目前，Interface的名字和ADT/Struct的名字可以重复，ADT和Struct的名字不可以重复
8. ```PatAs``` 不是很明白，这里需要检查一下
9.  binary op 稍微改一下
   ```OCaml
   type binary_op =
   | BinOpCompare of compare_op
   | BinOpLOr | BinOpLAnd | BinOpLNot
   | BinOpLShift | BinOpRShift
   | BinOpAdd | BinOpSub | BinOpMul | BinOpDiv | BinOpMod
   | BinOpAssign of binary_op option
   ```
11. See ```TODO ``` ```Discuss``` in ```name_resolute.ml```
12. 隐式类型转换么？还是需要显式？安全起见最好是提供库级的类型转换函数。
13. 关于函数体内部的return 返回类型检查，关于This类型检查，还没做，需要插入当前在哪一函数内的环境信息
14. ADT branch modify
    ```OCaml
    type adt_def =
    { adt_name     : typ_name
    ; adt_branches : (adt_label * typ list) list }
    ```
15. ```List.find_map``` OCaml v4.10+

