## Deeplang-lbnf
### 运行
```bash
# !bnfc required!
bash ./BASHME.sh
```
### 简介
```json
{
  "vertsion": "alpha",
  "author":"Mepy",
  "date":"20210810"
}
```
### 实现解析(parse)的部分
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
### 缺陷
```return Exp;```语句是出现在可以出现在所有地方

数组还没有实现。