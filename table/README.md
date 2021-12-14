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