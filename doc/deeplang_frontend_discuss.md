### deeplang编译器前端讨论

**codegen需要哪些信息**

- 类型信息
- scope信息

**编译流水线**

- lua，不生成ast，parse中生成符号表，启动速度块；
- haskell，4-5种ast；
- rust，mir borrow checking；
- deeplang，一颗ast树，type check，2+次遍历，模式匹配；
  - 自顶向下（scope信息、符号表）；
  - 自底向上（类型信息）；
  - 遍历2遍（codegen）；
- deeplang符号表
  - name，symbol name
  - type，function type，variable type
  - scope，
  - offset，
  - 代码坐标，

**deeplang非结构的控制流**

- abort
- exception