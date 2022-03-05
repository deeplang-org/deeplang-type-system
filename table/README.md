## Walk 1st, table
In root directory of the repo:
```bash
$ eval `opam config env`
$ bash BASHME.sh
$ test
```

refer to ```./table/example.ml```, ```./output/AbsDeeplang.ml``` and ```./output/ShowDeeplang.ml``` to develop

```bash
$ cd ./output
$ # currently, just use repl to dev
$ utop 
```
目前还没有引入node指针，感觉要在顶层（即tableCode函数中立即插入e:node aka AbsDeeplang.code），这里的所谓指针，其实其类型就是AbsDeeplang.code，相当于引用。

### 20220305 dev notes
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
6. 目前还没有检查成员函数与成员变量的重名情况，需要在```walk_method_decl```内修改
7. 目前，Interface的名字和ADT/Struct的名字可以重复，ADT和Struct的名字不可以重复
8. ```PatAs``` 不是很明白，这里需要检查一下
9. binary op 稍微改一下
   ```
   type binary_op =
   | BinOpCompare of compare_op
   | BinOpLOr | BinOpLAnd | BinOpLNot
   | BinOpLShift | BinOpRShift
   | BinOpAdd | BinOpSub | BinOpMul | BinOpDiv | BinOpMod
   | BinOpAssign of binary_op option
   ```
10. See ```TODO ``` ```Discuss``` in ```name_resolute.ml```
11. 隐式类型转换么？还是需要显式？安全起见最好是提供库级的类型转换函数。
12. 关于函数体内部的return 返回类型检查，关于This类型检查，还没做，需要插入当前在哪一函数内的环境信息
13. ADT branch modify
    ```
    type adt_def =
    { adt_name     : typ_name
    ; adt_branches : (adt_label * typ list) list }
    ```
14. ```List.find_map``` OCaml v4.10+