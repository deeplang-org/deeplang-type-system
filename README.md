## Deeplang Type System

We are learning TAPL and developing some interesting projects as follow:

- [untyped-lambda-calculus](http://mepy.net/untyped-lambda-calculus/) based on javascript
- [Dart-Lambda-Calculus](https://github.com/sorrowfulT-Rex/Dart-Lambda-Calculus) based on Dart

**Todo list:**

- Deeplang parser/lexer based on bnfc
- Deeeplang typing rules
- Deeplang symbol table
- Deeplang type checker and then type infer
- Deeplang wasm codegen

### LBNF

We use bnfc to generate deeplang parser and lexer. There is a file named `deeplang.cf` in repo. The file is a labelled bnf file.

#### Build
```bash
# !bnfc required!
bash ./BASHME.sh
```
#### Meta Data
```json
{
  "vertsion": "alpha",
  "author":"Mepy",
  "date":"20210810"
}
```
#### 实现解析(parse)的部分
- 注释 comment
- 声明 Dec
  - 变量(可变与否，类型)
  - 函数(签名)
  - 接口(继承父接口)
- 定义 Def
  - 函数
  - 类(继承父类，实现接口)
- 运算 Exp
  - 算术 Arith
    - \<\<
    - \>\>
    - \+
    - \-
    - \*
    - \/
  - 布尔 Bool
    - 比较 Cmp
      - 小于 Lt
      - 小等于 Leq
      - 大于 Gt
      - 大等于 Geq
    - 逻辑 NOT AND OR
  - 实例化 Instant
- 语句 Sta
  - 条件 Cdn
    if else，必须加括号，没有else-if
  - 循环 For
#### 缺陷
```return Exp;```语句是出现在可以出现在所有地方

数组还没有实现。