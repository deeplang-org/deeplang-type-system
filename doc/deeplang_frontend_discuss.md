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

练琪灏：最近跟着刘请的设计思路，名字检查、类型检查、编译流程的作业，基于库上ast 的做名字检查、symbol tab。typing rules，涉及委托、接口的类型规则，需要聚焦讨论。类型检查和普通模式匹配差不多，tuple。解构赋值。

陈楷骐：学习haskell和rust。了解类型系统。在看ast代码。尝试写一个带type system的demo。加到这dev分支。master，main。

陈亦棠：deepvm的bug，memory leak，flow control，builtin，虚表。ocaml环境搭建，bnfc功能有限，之后用手写的parser。

陈煜杰：ocaml的type用法，涉及deeplang type tab并写了一个小demo。type比较。

刘请：ast设计，内存管理系统设计。年后讨论内存方案。

魏韧韬：symbol tab相关实现，和琪灏合作。

祝大家新年快，虎年大吉。

### 2022.2.20例会纪要

练琪灏：同名var，func，method的处理的讨论。

1. 是否允许变量重名？允许同一作用域重名覆盖。
2. 变量名和函数名是否可以重名？函数和成员函数（方法）分开成两张表。同一个类型里成员变量和成员函数可以重名
3. 处理bnfc生成的代码告警处理。push

### 2022.2.27例会纪要

练琪灏：成员变量和成员函数的type检查。提供table的访问接口。

陈亦棠：刷deeplang.cf文件。

陈楷骐: 实现一个简单的表达式type检查，类型检查优雅报错（location）。

### 2022.3.27例会纪要

练琪灏：

陈亦棠：

陈楷骐:

### 2022.4.17例会纪要

练琪灏： 文档push到仓库，名字检查，类型检查。

陈亦棠： 加强针。基本语法做好了。for循环的问题。

陈楷骐:  大作业。

刘请：设计deeplang的内存管理系统。

魏韧韬：测试样例。



### 2022.4.24例会纪要

竺可桢学院的邱日宏同学加入。

练琪灏： NA

陈亦棠：for( : )换成 for( in )，

陈楷骐:  NA

刘请：ANF可以将普通变量和node id进行统一，解决重名问题。

魏韧韬：NA

遗留问题：if 和match是expression还是statement？

### 2022.5.1例会纪要

练琪灏：编译原理的课程设计。https://github.com/deeplang-org/deeplang-type-system/blob/dev/table/Walker.ml deeplang ast的遍历，需要review。

陈亦棠：刷新deeplang主仓的readme，最新代码会有PR。

陈楷骐:  CPU作业。

刘请：写borrowchecker的实现。已经提交一部分。表达式的borrowchecker已经写完。https://v2.ocaml.org/api/Parsing.html ocmal token的行列号。

魏韧韬：编译大作业。

秦嘉俊：bnf，bnfc，ocaml编程语言。

邱日宏：deeplang的讨论， ast树。

遗留问题：if 和match是expression还是statement？

### 2022.6.12例会纪要
framework:
- parser
- type checker
- codegen
- deepvm

done:
1. bnfc parser --
2. walker

todo:
1. dune project
2. parser + walker api
3. type checker
4. ANF trans 第一次AST遍历，解析作用域之前，简单的IR，后面可以基于ANF变换后的tree进行type checker，codegen，https://github.com/Mepy/tc/blob/master/src/ast/api.hpp， stat expr需要ANF，ANF的数据结构需要设计。
5. deeplang memory system，稍后计划。
6. codegen, wasm bytecode
7. deeplang highlighter plugin

https://guest0x0.xyz/deeplang-borrow-checker-demo/demo.html


### 2022.7.3例会纪要
1. ANF是否需要？
  - ANF变换不是必须的工作。
  - AST中已经有node_id，类似功能。
  - 如果做的话，AST node_id删掉，做ANF parser。
  - 收益不是很大，暂时不做。
2. Walker合入dune工程的进展？
  - AST适配，编译通过。
  - 正在测试。
3. 前端报错？
  - parser报错结构，ocamlyacc的报错机制
  - 增加错误语法匹配规则，调研一下ocamlyacc的报错机制（遗留问题）
``` ocaml
| TOK_LPAREN expr_list_nonempty TOK_RPAREN
    { match $2 with [expr] -> expr
                  | exprs -> mk_expr @@ ExpTuple exprs }
| TOK_LPAREN expr_list_nonempty error {error @@ Expecting "norihgt xxx"}
```
4. walker存放sema文件夹中单独的库。

### 2022.7.17例会纪要

报错规则讨论
进展：
    未完整代码报错
    句号.报错显示
    lexer错误报错基本覆盖
    parser错误信息大部分覆盖
        小部分等待开新的分支
    结构体中逗号，分号的判断

讨论：

报错目前比较简单，只有unexpected，expected ...

1、遇到哪个token出错的报错
    增加一个rule将string捕捉
    再看看Ocmal yacc自己的语法

2、希望message能提供猜测用户想要写什么东西
    例如，某个东西和另一个非常相似，然后就提供这样的报错

3、lexer和parser对于字面量过大的检测
    手动做计算检测字面量过大

4、Syntax阶段的报错
    比如用户自定义类型但未定义类型等待后续检查

5、逗号之后的逗号，左右括号不匹配等细化的错误信息
    使用带有局部输入的错误信息判断

6、token和label的区别：
    token：字面量，比如“operator”
    label：代表一类的token，比如operator中具体的+,-,* ...

7、变量变成表达式的方法
    单独变量，加圆括号变为函数调用，加点变为方法...
    变成表达式之后加分号
    如果要匹配的话就需要每种情况都进行匹配

8、看上去比较辅助性的错误可以先不必提供

9、第42行 _as mul y: [U8: 10] => return 12; 的return未成功高亮

10、多个下划线产生的错误
    特殊处理，如bad token



### 2022.7.31例会纪要

Deeplang项目安排

​	Deeplang是华为与竺院合作的一个深度科研训练项目，今年已经是开展的第三届了。在Deeplang第一届的开发中使用C++写了前端，但后续过程发现出现了一些问题。经过讨论和反复的选择之后，在第二届开始Deeplang前端改用Ocaml语言进行开发，但后端还是用C完成。

​	Deeplang是一个面向IoT，自主管理内存的语言。其手动管理内存的语言特性目前只有C以及Rust这样的语言拥有。目前的Deeplang语言是一个混合方案，除了有borrow-checker，C#值类型，还打算做原生引用计数等工作。

​	接下来，杨老师将暂时离开华为，前往江南大学开始博士阶段的学习。之后科研项目华为与竺院接口可能会有所改变，也许会需要与新的老师沟通交流。在之后的发展道路上，Deeplang可能会有些变化，但相信我们一定可以一起将Deeplang带向更好的明天。



作业对齐：

刘请：borrow-checker的加函数。对于该函数目前有两三种的实现方式，目前在根据论文参考选择一种优雅的方式，最后写进项目中。

陈楷骐：最近在做超算比赛，接下来会推进一些。

练琪灏：在看borrow-checker的部分，之后会跟进一些该部分。（附：希望能够在刘请整理之后看会更合适一些，命名上会更加可读）

秦嘉俊：熟练Ocaml语法了，可以和语法检查部分一起努力

邱日宏：了解Ocaml语法，在看type-system部分。

陈亦棠：暂时没有什么，之后会推进一些

刘得志：学习Ocaml，想要开始看deeplang的工程文档，完善deeplang语法高亮插件



学习建议：在Deeplang-type-system里doc中的simple compiler有一些例子，这些在doc文件夹里有些demo可以参考，适合可以入门学习

项目展望：语法报错完成之后，预计前端报错工作今年收尾，后端希望能够有些推进




### 2022.8.7例会纪要

进展汇报

​	陈亦棠：

​		增加了报错样例（文件置于Syntax/test/expressions等）

​		expression目前已完善许多

​	刘请：

​		内存管理系统中，指针的指针（借用的借用）压缩成一层的操作的实现研究。

​		在研究rust的相关操作，考虑如何用形式上最简单的方式实现，不行的话就进行开洞的操作。

​	邱日宏、秦嘉俊、刘得志：

​	陈楷骐：正在编译器比赛

​	魏韧韬：考托福，考虑去美国

​	练琪灏：写讲义和毕设调研



任务分工

​	邱日宏：pattern部分

​	秦嘉俊：statement

​	刘得志：一些细节部分，除了expression，pattern和statement部分之外的规则，基本直接加上error错误即可。



备注：

​	修改Parser.mly的内容之后需要在Syntax的dune文件和test里修改。此时，ParserTest里会同时测试正向用例和反向用例，如果返回OK则表示正常，否则需要进行一些修改。注意尽量不要产生conflict



### 2022.8.14例会纪要

陈亦棠：正在推进，打算部分完成后一起汇报

邱日宏：改进了一些高亮插件

秦嘉俊：补了一些编译原理的基本概念

刘得志：继续补一些概念

刘请：准备将一些demo重写一遍，已经完成一部分了

练琪灏：正在写一个助教讲义

陈楷骐：空闲下来了，打算在deeplang中AST等部分帮帮忙

魏韧韬：目前还在托福与GRE



### 2022.8.21例会纪要

陈亦棠：所有的内容放在了Syntax下，所有的反向用例放在了test下。演示并运行了所有反向测试用例。

邱日宏：尝试编写pattern代码，关于报错系统的一些疑问与解答。



建议：

let foo = %3; 的报错信息可以更加优化一些，提示这为二元运算符

参数列表的报错信息补全后再验证 let x = foo(1,  的报错信息位置



Deeplang语法复习：

方法名和字段名都是小写开头，如果大写开头则会提示错误



报错信息编写提示：

​	在Parser.mly文件的功能为构建起一棵抽象语法树，其中抽象语法树节点的定义信息可以参考Syntax/ParserTree.ml文件。在Parser匹配模式时，会优先匹配所定义的规则，最后均匹配不上时就会寻找error，并执行error中的命令实现错误信息提示。其中，外层的error为OCaml的token，而内层的error为Syntax/SyntaxError.ml文件中定义的error类型。在报错时从中选择合适的类型即可。



### 2022.8.28例会纪要

**lambda演算（练琪灏）：**

无类型的λ演算

​	图灵等价性（建模布尔值等）

​	参考文献：《Practical Foundations for Programming Language》

**borrow checker demo进展（刘请）：**

​	不捕获变量的递归加入后就基本完成编译器中需要使用的borrow checker部分的内容。

​	相关内容在GitHub deeplang/borrow-checker-demo的仓库链接中可以查看。



### 2022.9.4例会纪要

陈楷骐：pretty-printer编写过程中似乎需要AST的结构，还是需要scope中的内容。通过worker的id查询scope

​	需求：进入scope后打印scope的变量。当然，目前也可以先做一个比较简单的pretty printer，暂时不考虑脱离出来的额外的语法信息。当有更多东西需要debug之后可以完善pretty printer提供内部信息。

​	pretty printer也是在语义报错之前需要完成的。



秦嘉俊：正在推进statement部分的错误信息，提供了缺失分号等常规情况。

​	错误情况的经验：根据工业语言的错误信息研究报错情况，可以进行一定的参考，比如：Java，Rust等语言。在成熟语言的基础上进行改进。



邱日宏：pattern部分错误信息基本完成，相关更完善的内容可以在后续进一步改进。



刘请：完善borrow-checker，正在考虑如何加入到主线分支之中。



ANF的再讨论：

​	内建的引用计数，原计划在编译的时候使用引用计数的优化。在现有的框架下，精确的插入引用计数需要插入到表达式里。如果使用了ANF的话就可以直接插入，会更简单一些。

​	不过，在目前因为有node id存在，也可以完成引用计数的工作。暂时不做ANF也没有关系

### 2022.9.18例会纪要
陈楷骐：
刘得志：新的PR，else 报错，碰到一个问题ADT。
练：
刘请：还有循环要设计，PL的个人项目，后面讨论borrow checker 如何合入主干。
陈：课程，阅读expr。
魏：类型系统测试样例。

遗留：
1、 readme中的ADT中去掉name

****



### 2022.9.25例会纪要

邱日宏：这周在补上周参加竞赛时落下的内容，还没太多进展

练琪灏：最近在忙面试，计划加一个模式匹配

陈楷骐：还在写pretty printer

刘请：刚完全borrow checker并完成测试。计划接下来

刘得志：不太清楚接下来的该做什么

秦嘉俊：刚开学还没有太多进展



### 2022.10.2例会纪要

刘请：borrow-checker有新的进展，演示了相关程序结构中借用等部分的Demo

邱日宏：接下来开始看Semantics中的walker部分，继续完成前端中语义报错的内容

陈楷骐：pretty printer快写完了，预计下周可以完成

练琪灏：在处理语言中的delegate的特性



### 2022.10.16例会纪要

**Deeplang的codege设计**

Deeplang Source Code -> walker -> ast&tabs -> codege -> DeepVM -> libc

其中，codege设计中，考虑先将ast转为S表达式，接着自己实现wat做一个中间的文本形式。但可能会遇见维护成本的问题。

如果使用S表达式，S expression和ast更接近一些，文本形式的wasm文件和字节码之间使用官方库相互转换。

如果不生成S expression的话就要整体使用OCaml调用C语言的wasm库的接口来实现生成字节码bit stream。

参考：OCaml与C的互操作

优势：调用OCaml官方的库，如果官方库修改后我们无需有大的改动。OCaml和C的相互调用较为简单。



对齐各人的进展

陈楷骐：交了一个branch

秦嘉俊：今天下午已经将之前的工作合入了dev分支

刘得志：

​	两个问题：

​		只要是大写字母开头都会认为是类型，无法定义“未定义类型”

​		函数没有右括号的语句无法检查



### 2022.10.23例会纪要

关于是否做ANF变换的讨论

​	ANF是一种会将嵌套表达式拆开的IR

​	作用域检查

​		不做ANF：每个节点有唯一的node_id并存在哈希表

​		做ANF变换：变量名字全局唯一

​	借用检查

​		不做ANF时，借用需要很长一串的其他信息才能找到内部变量；在借用时，只允许对左值借用。但非左值的借用有时也是有意义的。

​		如果做了ANF，一些表达式引用会将嵌套表达式消去并当成一个变量，相当于自动插入了一个变量

​	代码生成

​		不做ANF：在codegen时， 一些临时表达式可能需要作为临时表达式来为其生成释放内存的代码。必须维护临时表达式的列表

​		做ANF：内存释放代码，临时表达式已被替换，不存在问题。插入引用计数操作可以有很多的优化



​	讨论：

​		ANF什么时候做？

​		做ANF的时候可以顺便做的东西？



Deeplang的codegen设计

​	从deeplang的语法一步步转到wasm上

​	如果像C那样的数组能够做的出来，那么其他也可以做出来。否则需要拆成多个独立整数变量做。

​	interface可以在早一些做完，codegen再完成可能有点晚

​	闭包函数也不必考虑，只需要考虑全局函数



### 2022.10.30例会纪要

codegen

现状：在wasm1.0中还没有结构化数据类型，连数组都没有

解决方案1：在wasm的S表达式中自己构造。

​	wasm中存在类似指针偏移量存储数据的功能，实际实现可能会更简单一些。



ANF变换

确定了ANF IR需要完成的事情

形成了IR的初稿，梳理了一下代码内容



### 2022.11.6例会纪要

基于DeepVM的load和store的tuple实现：

​	对于tuple数据除了字符串之外可以直接采用I32或I64直接存储，而字符串由于可能较长，可以存在VM中的.data字段中（只能是read-only），或者是存在tuple之中（可修改）。



IR的修改

* 没有goto，简化IR
* Deeplang有类似指针的读写借用，但WSM不太支持
* local局部变量等的存储



讨论了Deeplang后端的设计演变，以及WASM的相关内容
