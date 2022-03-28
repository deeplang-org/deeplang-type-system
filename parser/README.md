# Parser使用办法

1. 如果parser目录下没有Makefile，执行`bnfc --ocaml -m deeplang.cf`，否则跳过此步。
2. `make`。
3. `./TestDeeplang [源文件.deeplang]`；在parser/test目录中有许（yi）多（ge）测试样例。
