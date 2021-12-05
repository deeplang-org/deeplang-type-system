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

#### 陈楷骐typing rule的问题：

- tuple、array同时存在的必要？
- char、string同时存在的必要？
- 常量类型处理char、int等类型；字面量类型；
- 函数不是一等公民
- 可变性的问题，等待设计稳定后
- interface和委托正在看；

| 模块                            | 负责人         |
| ------------------------------- | -------------- |
| walker                          | 练琪灏         |
| typecheck                       | 陈楷骐         |
| symboltable                     | 魏韧韬，练琪灏 |
| types/typeconstruct/typecompare | 陈煜杰         |

### 符号表字段：

#### 1、符号表最基本的字段：

符号表，vectors of symbols

sym_id: 符号的id，全局唯一；

**name**：符号的名字，比如声明dcl，identifier（变量名、函数名等）

**scope_ref**：指向上一个作用域的指针或引用；

scope_level：作用域的，top level=0；

node*：ast节点的引用，name --> node  映射，可以找到 scope；

ty：语义的type，比如函数签名；

target：ref-expr，ref-type，指向它定义的id。

**索引方案：**

1、map(name, scope_ref)  ->记录

2、id -> 记录

每次变量产生一张符号表，索引都是按照sym_id。

第一次遍历：(name, scope_ref) ，sym_id塞到ast的node;

第二次遍历：按照sym_id，填type检查得到信息;

**问题**：

1、重名变量的问题？

- 连续的for循环，相同变量 i；

2、ast节点的表以及变量sym的表分开？

3、类型和作用域等分开存放在不同表中？

#### 2、其他类型的字段

