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

| 模块                            | 负责人             |
| ------------------------------- | ------------------ |
| walker                          | 练琪灏 Mepy        |
| typecheck                       | 陈楷骐             |
| symboltable                     | 魏韧韬，练琪灏Mepy |
| types/typeconstruct/typecompare | 陈煜杰             |

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

### 符号表的设计：

**sym_tab1**: 作用域；

**sym_tab2**: 类型，语义信息；

**第一次遍历**

**dcl_tab**

| feild       | data type             | backup   |
| ----------- | --------------------- | -------- |
| sym_id      | int                   |          |
| name        | string                |          |
| scope_ref   | ptr or something else |          |
| scope_level | int                   |          |
| node        | ptr or something else | 暂时保留 |

**ref_tab**

f (x, y+z) : ref_tab只有 f 和 x 两个，没有y和z。

只存**叶子节点**的ref信息。

行号和列号的生成问题，node_id的生成问题，需要在parse阶段实现。

| feild   | data type | backup                              |
| ------- | --------- | ----------------------------------- |
| node_id | int       | ast每个节点的ID。                   |
| target  | int       | target, defination, dcl tab->sym_id |

ast结构：expr kinds 进行工作拆分，声明（函数签名）、函数体/类/变量定义、表达式、控制语句。

周二，输出遍历框架代码。

**第二次遍历**

```deeplang
let foo : Foo = new Foo ( );
```

ty tab

| feild  | data type | backup         |
| ------ | --------- | -------------- |
| sym_id | int       |                |
| ty     | type      | 用户的，系统的 |

类型判断相等：ty判等



### 2022.1.9例会纪要

expr和statement是否统一成expression，结论：不合并。

遗留：

1、流程控制在expr和stat设计两套，

2、node_id, symbol_id拆成更细的table，支持刘请设计

讨论到了patterns；

### 2022.1.16例会纪要

1、类型名字，接口名字，函数的名字，不重名。

2、不要++，--；

3、不要构造函数；

4、刷deeplang spec，加命名规则（大小）；

5、for loop，支持 range iterator；

6、我们只有type checker，要标注类型；



stream parser，二进制匹配操作，match，collect

head：fe aa

length: 06

payload: 01 02 03 04 05  06

tail: 3d ef

crc or lrc: 98

### 2022.1.23例会纪要

练骐灏：最近跟着刘请的设计思路，名字检查、类型检查、编译流程的作业，基于库上ast 的做名字检查、symbol tab。typing rules，涉及委托、接口的类型规则，需要聚焦讨论。类型检查和普通模式匹配差不多，tuple。解构赋值。

陈楷骐：学习haskell和rust。了解类型系统。在看ast代码。尝试写一个带type system的demo。加到这dev分支。master，main。

陈亦棠：deepvm的bug，memory leak，flow control，builtin，虚表。ocaml环境搭建，bnfc功能有限，之后用手写的parser。

陈煜杰：ocaml的type用法，涉及deeplang type tab并写了一个小demo。type比较。

刘请：ast设计，内存管理系统设计。年后讨论内存方案。

魏韧韬：symbol tab相关实现，和骐灏合作。

祝大家新年快，虎年大吉。

### 2022.2.20例会纪要

练骐灏：同名var，func，method的处理的讨论。

1. 是否允许变量重名？允许同一作用域重名覆盖。
2. 变量名和函数名是否可以重名？函数和成员函数（方法）分开成两张表。同一个类型里成员变量和成员函数可以重名
3. 处理bnfc生成的代码告警处理。push

### 2022.2.27例会纪要

练骐灏：成员变量和成员函数的type检查。提供table的访问接口。

陈亦棠：刷deeplang.cf文件。

陈楷骐: 实现一个简单的表达式type检查，类型检查优雅报错（location）。

### 2022.3.27例会纪要

练骐灏： 

陈亦棠： 。

陈楷骐:  。

### 2022.4.10例会纪要

练骐灏： 

陈亦棠： 

陈楷骐:  





