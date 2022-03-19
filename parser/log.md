# 日志

## 2月28日

开始按照主仓的README处理Expression和Statement。

1. Expression中是否还需要Macro？（暂时注释掉了）

2. 不知道元组是否已经实现

3. 添加了While语句，按照README修改了模式匹配语句。结构体部分可能还不对

4. 将`(mut)? VarId : Type`这种形式提取出来（因为很常用）

## 3月3日

1. 处理好了所有的基本类型

2. 需要函数类型么（`Type -> Type`)？

3. Tuple的格式是`(Type, Type, ..., Type)`么（任意个`Type`）？

4. 处理好了结构体，但是没有方法。不知道结构体允不允许有方法，如果有的话，应该是什么格式？

5. ADT似乎目前符合README，没有修改

## 3月19日

1. 处理好了Interface声明/实现
