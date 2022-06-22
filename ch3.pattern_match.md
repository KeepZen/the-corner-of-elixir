# 模式匹配

## 模式匹配的作用
模式匹配完成以下的两个工作:

1. 确认数据具有某种模式
2. 以某种模式, 从数据中提取信息

在其他语言中, 这两个功能有着清晰的分界, 但是在 Elixir 中, 有清晰分界的情况是少数的,
大部分是两个功能混合在一起的.

## 模式确认
Elixir 特意提供了一个专门完成模式检查工作的宏 `match?/2`. 这个宏返回的是一个布尔值.

```elixir
true = match?({:ok, b}, {:ok, 1})
:b not in (binding()|> Keyword.keys())
false = match?({:ok, _}, :error)
```

上面的代码, 第 1 行表明, 当 `match?` 第一个参数作为模式, 可以匹配第二个值得时候,
返回值是 `true`. 第 2 行, 通过检查 `binding()`, 确认即使匹配成功,
这个宏也并不在环境中引入新的变量. `binding/1` 是一个宏, 返回指定的上下文函数的绑定值.
`biding/0` 返回当前环境中的绑定情况. 这个函数返回的是一个 Keyword.
代码  `binding()|>Keyword.keys()` 返回了当前的上下文中所有的变量 (以原子表示).

第 3 行, 展示了当 `match?` 的两个参数匹配失败的时候,
返回 `false` 而不是引发一个异常.

和 `match?/2` 完成的工作非常相似, 但是只专注于字符串匹配的是操作符 `=~`.
`=~` 返回的也是布尔值.

```elixir
true = "hello" =~ "hello"
true = "hello" =~ ~r/hel.{2}/
```

对参数的要求, `Kerner.match?/2` 和匹配操作符 `=` 一样, 第一个参数作为模式,
第二个参数作为需要匹配的值. 字符匹配操作符 `Kernel.=~/2` 与它们不一样.
`=~/2` 的第一个参数是要匹配的数据 --- --- 具体说是字符串,
第二个参数是模式: 字符串或者正则表达式. 这是一点小不同.

其实, 我更希望能够使用操作符, 来完成更多的模式确认的工作.
`=` 在 Elixir 和 Eralng 中已经被命名为模式匹配操作符了, 为了避免混淆,
我把将要定义的 `=~/2` 宏, 叫做 **模式确认操作符 (Pattern Confirm Operater)**.

将要定义的模式确认操作符 `=~` 除了完成字符串匹配外,
还可以完成其他的模式确认的工作.

如果模式确认操作 `=~/2` 接受的第一个操作符是字符串,
那么就代理  `Kernel.=~/2` 函数来执行字符串匹配.
为了和 `Kernel.mathc?/2` 的参数顺序保持一致, 
我们还支持正则表达式字面量 (`~r` 或 `~R`) 作为第一个参数,
字符串作为第二个参数, 来完成模式确认的工作.

其他情况, 全部有 `Kernel.match?/2` 来完成.

但是 `Kernel.match?/2` 有一个小小的问题:
当我们使用一般的变量来作为匹配的模式时, 编译器会给出恼人的警告.
例如 `match?([a,b],[1,2])` 会警告: 变量 `a` 和 `b` 都没有被使用.
实际上,  就像我们上面已经解释过的, `match?([a,b],[1,2])` 
根本不向运行时环境引入变量, 那么这样的警告就显得有些古怪.

为什么 `match?/2` 要给出这样的警告呢?

想想看, 如果首先定义了一个变量, `a=[1,2]`, 然后期望当 `value` 的值为 `[1,2]` 时,
`match?(a,value)` 返回 `true`. 这样的代码, 正确吗?
因为 `match?` 接受的第一个参数是一个模式, 而变量是可以匹配任意值的,
所以无论 `value` 的值是什么内容, `match?(a,value)` 都会返回 `true`.
要表达按照变量的值来匹配, 正确语法是 `match?(^a,value)`.
这就是为什么 `match?/2` 遇到第一个参数中有不以下划线开头的变量时, 给出警告的原因.
对于这个问题, 模式确认操作符时这样处理的, 模式操作符的第一个参数是一个变量,
那么抛出一个错误. 其他的情况, 在把模式传递给 `match?/2` 前,
给普通的变量加上下划线前缀, 这样就能最大限度的消除 `match?/2` 给出的古怪警告.

现在让我们看看如何实现.

## 模式确认操作符的实现
模式确认操作符, 是要覆盖 `Kernel.=~/2` 这个函数的. 
新定义的宏, 兼容了 `Kernel.=~/2` 这个函数, 使用者不用关心其使用的 `=~/2` 到底是
`Kernel.=~/2` 还是我们定义的宏. 为了方便使用者, 我们在模块中定义 `__using__/1` 宏.
```elixir
defmodule Corner.PatternConfirmer do
  defmacro __using__(_opt) do
    quote do
      import Kernel, except: [=~: 2]
      import Corner.PatternConfirmer, only: [=~: 2]
    end
  end
  #...
end
```
这样, 在使用的时候, 只需要一个 `use` 语句 `use Corner.PatternConfirmer`
就可以完成宏导入的工作. 上面的代码片段是非常常见的 `__using__/1` 宏的写法.
这个宏完成的实际工作也非常的直观. 可以认为, 它完成了 C 或 Erlang 宏的文本替换的工作:
在编译时, 用 `quote do..end` 中的两个 `import` 语句替换了
`use Corner.PatternConfirmer`.

模式确认操作符 `=~/2` 定义稍微复杂一点.
根据第一个参数, 也就是我们要确认的模式, 可以分成 4 中情况:
1. 模式为字符串
2. 模式为正则表达式常量
3. 模式为一个单独的变量
4. 其他情况

第一种情况最简单, 我们的宏只需要代理 `Kernel.=~/2` 函数就可以了.
第二种情况, 稍微复杂一点, 主要是对正则表达式的确认上有一点点的麻烦.
对这两种情况的处理代码为:
```elixir
defmacro left =~ right do
  cond do
    is_binary(left) ->
      text_match(left, right)

    (match?({:sigil_r, _, _}, left) or
       match?({:sigil_R, _, _}, left)) and
        is_binary(right) ->
      text_pattern_confirm(left, right)
  #... other condtion
  end
end
```
不难猜到 `text_match` 和 `text_pattern_confirm` 的代码:

```elixir
defp text_match(left, right) do
  quote do
    Kernel.=~(unquote(left), unquote(right))
  end
end

@compile online: true
defp text_pattern_confirm(left, right) do
  text_match(right, left)
end
```
`text_match` 和 `text_pattern_confirm` 作的工作是一样,
只是参数的位置做了调换. 所以要如此写, 是因为这时两个工作.
`text_match` 完成的是对 `Kernel.=~/2` 函数的兼容,
`text_pattern_confirm` 是以正则表达式为模式来作模式确认的.

第 3 个情形:  对单独的变量作模式的处理, 和其他都不一样.
其他情形, `=~/2` 返回的总是 `true` 或 `false`. 第三种情形,
一定是一个编码疏忽, 所以, 这种情形, `=~/2` 抛出一个错误,
提示语法错误.
相关代码如下:
```elixir
defmacro left =~ right do
  cond do
    # text_match and regex_confirem
    match?({_atom, _, _}, left) ->
      singal_variable_pattern_handle!(left, right)
    #other condintion ...
  end
end
# ...
defp singal_variable_pattern_handle!({atom, _, _}, _right) do
  quote do
    message =
      "The first paramse of pattern confirm operater should " <>
        "be a pattern, but get a variable `#{unquote(atom)}`, "<>
        "this will be aways match."
    raise SyntaxError, message
  end
end
```
像上面已经解释的那样, `singal_variable_pattern_handle!/2` 就只是产生一个异常.

最复杂的是对其他模式的处理. 这里的困难在于如何为普通的变量添加下划线前缀.

相关代码为:
```elixir
defmacro left =~ right do
  cond do
    # text_match
    # regex_confirem
    # single_variable_hanlde
    
    true ->
      other_pattern_confirm(left, right)
  end
end

defp prewalker({:^, meta, args}) do
  args =
    Enum.map(
      args,
      fn
        {atom, meta, value} -> {atom, [:exclude | meta], value}
      end
    )
  {:^, meta, args}
end

defp prewalker({atom, [:exclude | meta], value}),
  do: {atom, meta, value}

defp prewalker({atom, meta, value} = ast) when is_atom(atom) do
  if "#{atom}" |> String.starts_with?("_") do
    ast
  else
    atom = "_#{atom}" |> String.to_atom()
    {atom, meta, value}
  end
end
defp prewalker(ast), do: ast
```

要完成对模式中变量的修改, 我们需要对模式的抽象语法树作遍历.
这里使用 `Macro.prewalk/2` 来对抽象语法树执行前序遍历.

如果存在 `^a` 这样的表达式, 那么会首先执行第一个 `prewalker/1` 分句,
这个分句是为了在之后的遍历中, 区分 `^a` 和 `a` 而作的准备工作.
这个分句为, 表达式 `^a` 中的 `a` 的抽象语法树,
添加元数据,  `:exclude`, 以标记这个 `a` 不需要转化为 `_a`.

第二个 `prewalker/1` 分句处理标记过的变量的抽象语法树, 去掉添加的 `:exclude` 标记,
还原为起初的样子.

第三个 `prewalker/1` 分句处理没有被标记为 `:exclude` 的变量的抽象语法树.
如果变量名以 `_` 为前缀, 那么不用处理; 否则为变量添加 `_` 前缀. 

最后一个 `prewalker/1` 分句对所有其他的抽象语法树, 保持不变原样返回.

`Macro.prewalk/2` 在调用第一个分句后, 对其返回的结果, 会继续使用之后的分句作遍历.
后面的三个分句中, 有一个会执行. 这样我们就完成了对普通变量添加 `_` 前缀的工作.

最后, 让我们测试以下我们的代码:
```elixir
defmodule PatternConfirmerTest do
  use ExUnit.Case, async: true
  doctest Corner.PatternConfirmer
  use Corner.PatternConfirmer

  test "string =~ regex" do
    t = "hello" =~ ~r/hell/
    assert t == true
    f = "hello" =~ ~r/wolrd/
    refute f
    regex = ~r/hel{1,2}o/
    t = "hello" =~ regex
    t2 = "helo" =~ regex
    assert t and t2
    f = "world" =~ regex
    refute f
  end

  test "regex =~ string" do
    t = ~r/hel{1,2}o$/ =~ "hello"
    assert t == true
    t = ~r/hel{1,2}o$/ =~ "helo"
    assert t
    f = ~r/hel{1,2}o$/ =~ "world"
    refute f
  end

  test "patter_just_a_varibal" do
    assert_raise Corner.PatternConfirmer.SyntaxError, fn ->
      quote do
        use Corner.PatternConfirmer
        regex = ~r/hel{1,2}o/
        regex =~ "hello"
      end
      |> Code.eval_quoted()
    end
  end

  test "other_patter =~ value " do
    t = [a, b] =~ [1, 2]
    assert t
    a = 1
    array = {1, 2}

    f = {^a, b} =~ array
    assert f
    t1 = 1 =~ 1
    assert t1
    t = {1, _} =~ {1, 2}
    assert t

    [f1] = [
      1 =~ 2
    ]

    refute f1

    f2 = [1, _] =~ {1, 2}
    refute f2
  end
end
```

注意这里的测试代码, 必须把宏调用的结果存放到变量(这里是 `t` 或 `f` 等等)里,
然后使用宏 `assert`/`refute` 判定这个变量的值. 直接使用 `assert 1 =~ 1` 会报错.

在 &Lt;Elixir Meta Programming&Gt; 这本书中, 作者对宏测试给出的建议是:
测试宏的结果, 而不是测试宏如何工作.

把宏的结果, 放在变量中, 然后判断这个变量, 就是测试宏的结果.

这是一个结论, 所以如此原因比较复杂. 对我们的例子来说,
所以不能直接使用 `assert/refute` 来测试, 是因为 `assert`/`refute` 也是宏,
且它们会改变其后的表达式 `pattern =~ expection` 的抽象语法树,
从而影响待测试的模式确认操作符 `=~/2` 的输入.

这是 `assert/refute` 宏的 bug, 这个 bug 的修复工作, 在 Elixir 1.13.4
版本中已经启用.
## 按模式提取信息

和 `match?/2` 以及 `~=` 提供的功能刚好相反, Elixir 中也提供了,
专注于按模式提取数据的语法结构:

1. `destructure/2` 针对列表
2. `get_in/2` 针对 Access 行为

这些函数的用法, 在 Elixir 的文档中有详细的说明,
我不能解释的更好. 如果你 (读者) 不明白, **他们是用来提取数据的**,
是什么意思, 那么建议读一读 Elixir
[destructure](https://hexdocs.pm/elixir/Kernel.html#destructure/2) 和
[get_in](https://hexdocs.pm/elixir/Kernel.html#get_in/2) 的文档.

我喜欢 `descructure` 这个函数, 它让我怀念 Javascript 的解构操作符 `...`.
可惜 `descruture/2` 这个函数只支持对列表的解构.
那么我们来定义自己的解构操作符吧.

```elixir
defmodule Corner.Desructure do
  defmacro left <~ right do
    cond do
      match?({:%{}, _, _}, left) ->
        destructure_map(left, right)

      is_ast_tuple(left) ->
        destructure_tuple(left, right)

      is_list(left) ->
        quote do: Kernel.destructure(unquote(left), unquote(right))

      true ->
        quote do: unquote(left) = unquote(right)
    end
  end

  defp is_ast_tuple({:{}, _, _}), do: true
  defp is_ast_tuple({_, _}), do: 2
  defp is_ast_tuple(_), do: false

  defp destructure_map(left, right) do
    default_val = make_default(left)

    quote do
      unquote(left) = Map.merge(unquote(default_val), unquote(right))
    end
  end

  defp make_default(ast) do
    Macro.postwalk(ast, &varable_to_nil/1)
  end

  defp varable_to_nil({atom, _, tag})
       when is_atom(atom) and tag in [Elixir, nil],
       do: nil

  defp varable_to_nil(v), do: v

  defp destructure_tuple(left, right) do
    left_size = ast_tuple_size(left)

    quote do
      right = unquote(right)
      diff = unquote(left_size) - tuple_size(right)

      patch_right =
        case diff do
          0 ->
            right

          # left < right
          diff when diff < 0 ->
            Tuple.drop(right, diff)

          # left > right
          diff ->
            fun = fn
              f, tuple, 0 -> tuple
              f, tuple, n -> f.(f, Tuple.append(tuple, nil), n - 1)
            end

            fun.(fun, right, diff)
        end

      unquote(left) = patch_right
    end
  end

  defp ast_tuple_size(tuple_ast) do
    case is_ast_tuple(tuple_ast) do
      2 -> 2
      true -> length(elem(tuple_ast, 2))
      false -> raise "require a tuple, this is not a tuple."
    end
  end
end
```

让我们测试一下:

```elixir
ExUnit.start(auto_run: false)

defmodule DesructureTest do
  use ExUnit.Case, async: true
  import Corner.Desructure

  test "desturcture from map literal" do
    %{a: a, b: b} <~ %{a: 1, b: 2}
    assert a == 1
    assert b == 2
    %{a: a} <~ %{}
    assert a == nil
  end

  test "desturcture from map value" do
    map = %{a: 1, b: 2}
    %{a: a, b: b} <~ map
    assert a == 1
    assert b == 2
    map = %{}
    %{a: a} <~ map
    assert a == nil
  end

  test "desturcture nest from map not work very well" do
    map = %{a: %{b: 1}, c: 1}
    %{a: %{b: b}, c: c, d: d} <~ map
    assert b == 1
    assert c == 1
    assert d == nil

    assert_raise MatchError, fn -> %{a: %{c: c}} <~ map end
  end

  test "desturcture from tuple literal" do
    {a, b} <~ {1, 2}
    assert a == 1 and b == 2
    {a} <~ {}
    assert a == nil
  end

  test "desturcture from tuple value" do
    tuple = {1, 2}
    {a, b} <~ tuple
    assert a == 1 and b == 2
    tuple = {}
    {a} <~ tuple
    assert a == nil
    {a, b} <~ tuple
    assert a == nil and b == nil
    {a, b, c} <~ tuple
    assert a == nil and b == nil and c == nil
  end

  test "desturcture nest from tuple not work very well" do
    tuple = {1, c: 1}
    {a, [c: c], d} <~ tuple
    assert a == 1 and c == 1 and d == nil
    assert_raise MatchError, fn -> {a, [c: c, d: d]} <~ tuple end
  end

  test "destructure list as Kernel.desturcture" do
    [a, b] <~ [1]
    assert a == 1 and b == nil
  end

  test "<~ work as `=` for varable" do
    a <~ {}
    assert a == {}
  end
end

ExUnit.run()
```

我们定义的新的操作符 `<~`, 对于列表, 元组和 Map 的单层结构, 可以完美的解构,
但是对于嵌套结构的支持还只是停留在了, 模式匹配操作符 `=` 的基础上,
就像 `destructer/2` 一样.

## 模式匹配操作符

Elixir 中使用最频繁的操作符是匹配操作符 `=`. 它的工作逻辑如下:

最常见的情形下, 按照 `=` 左侧的模式, 从右边的数据中提取信息.
但是当左边变量前中出现  pin 操作符 `^` 的时候, `=` 只完成模式确认的工作:
默认匹配成功了, 返回模式匹配操作符 `=` 右侧表达式的值; 而如果模式匹配失败了抛出异常.

在 Erlang OTP 25 中引入了一个新特性 maybe, 其中可以使用新的短路操作符 `?=`[^maybe],
叫做条件匹配操作符. 也许不久 Elixir 也会引入.
这个操作符实际上就是 `match?/2` 和模式匹配操作符 `=` 的混合体; 当模式匹配成功的时候,
完成变量的值绑定, 模式匹配失败, 返回 `?=` 左边的值.

[^maybe]: 见 [&Lt;Erlang 参考手册&bullet;maybe &Gt;章节](https://www.erlang.org/doc/reference_manual/expressions.html#maybe)

可见操作符 `=` 和 `?=` 都是混合了模式匹配的两种用法.
他们之间的差异在于, 当模式匹配确认失败后如何处理控制流和操作符的返回值.

## 逻辑短路与控制结构的分类

在执行逻辑操作的时候，有是否有支持短路操作的问题,
比如逻辑操作 `and` 和 `or` 在 Eixir 和绝大多数的编程语言中,
都是短路操作: 逻辑操作符号 `and` 只有当左侧表达式的结果为 `ture` 的时候,
右边的表达式才会求值; 而操作符 `or` 只有当左边的表达式结果为 `false` 时,
才会对右边的表达式求值.

但不是并不是所有的编程语言都是这样的, Erlang 中就提供了全路的逻辑操作,
当然也有对应的短路操作符号.
Erlang 中的 `and` 和 `or` 对逻辑操作符执行全路操作, 也就是说,
在 Erlang 中 `and` 和 `or` 的两个操作的值, 都会被计算一次;
而执行逻辑短路操作的是关键字 `and_also` `or_else`.
也就是说 Elixir 中的 `and` 和 `or` 对应的是 Erlang 的 `and_also` 和 `or_else`.
Elixir 中缺少全路的逻辑操作.

短路和全路操作, 对于逻辑结果来说是没有影响的, 有影响的只是副作用.

副作用的存在, 使得逻辑操作符可以用来充当控制结构.
这就是为什么要讨论逻辑操作短路与否的问题。
为了进一步的利用逻辑的短路操作做控制结构, Elixir 还提供了接受 `boolean_as` 类型的
操作符 `&&` 和 `||`.
C 语言的三目条件表达式 `cond ? true_part : false_part`,
在 Elixir 中既可以使用 `if cond,do: true_part, else: false_part`
又可以使用 `cond && true_part || false_part` 来模拟.

如果存在多个匹配模式, 以什么样的逻辑来处理这些模式的确认结果呢?

Elixir 的控制结构中, 使用的都是 `or` 的逻辑,
也就是说按照顺序对每个模式做一一确认, 直到发现了确认的模式.
但是当所有的模式全部失败后, 如何处理, 又可以有不同. 就像 `=` 和 `?=` 的差别那样.
我把 `=` 的选择叫做悲观主义, 而 `?=` 的选择叫做乐观主义.
那么控制结构, 可以分成三类:

1. 悲观的
2. 乐观
3. 特殊

## 悲观控制结构

Elixir 的大部分的控制结构都是悲观的: 即所有的模式匹配确认, 都失败后,
产生错误, 中断控制流. 悲观控制结构包括:

* 函数分句
* `case`
* `cond`

这些控制结构的大体的工作逻辑是这样的: 首先对模式做匹配, 如果不匹配, 接着匹配下一个模式,
如果所有的模式都不匹配, 类似于模式匹配操作符 `=` 匹配失败那样, 抛出一个错误;
而一旦匹配成功了, 还要完成数据提取的工作, 并执行对应的代码段.

### 函数分句

函数的分句这种概念, 从编程的效果上看, 非常类似于 C++ 和 Java 中的函数和方法的重载.
重载和函数分局都完成了对代码的动态调度. 但是这里不只是术语的名称的不同,
函数重载和和函数分句之间的差别更多是世界观的差别.

函数的重载可以发生在很多地方, 不只是限制在同一个命名空间中,
而且每个函数实际上也被认为是独立, 可区分的单元,
是编译器为程序员做了根据输入参数的不同区分和正确加载调度对应函数的工作.

Elixir 中函数分句只能发生在同一模块中, 而且这些函数应该集中放在一起, 
不应该被其他代码分隔开来. 例如下面的代码, 我们故意在 `fib/2` 的两个分句之间,
定义了一个新的函数 `b`, 这样的代码编译器会给出警告.

```elixir
defmodule CauseDemod do
  def fib(n, acc \\ [1, 1])
  def fib(0, [a, _]), do: a
  def b(), do: nil
  def fib(n, [a, b]), do: fib(n - 1, [a + b, a])
end
```

从语法基因来说, Erlang 中函数分句, 语法上是不能分开写的.
上面的代码, 对应的 Erlang 版本是这样的:

```erlang
-module('Elixir.CauseDemode').
-export([fib: 2, fib: 1, b: 0]).

fib(0,[a,_]) -> a;
fib(n,[a,b]) -> fin(n-1,[a+b,a]).

fib(n)-> fib(n,[1,1]).

b() -> nil.
```

Erlang 中, `.` 是语句结束的标志, 所以 Erlang 中是没有办法把函数分句拆散的.
但是 Erlang 中不允许, 不代表 Elixir 中不可以,
Elixir 对 Erlang 的语法改进是相当大的,
所以这里所以要给出警告, 绝对不只是为了与 Erlang 语法对应的问题.
而且编译器给出的是警告不是错误, 这本上就说明, 这不是技术实现上的问题.
那么警告的原因是什么呢?

这个要求首先是对函数分句概念呼应. 其次把相关代码紧凑的放在一起, 也使得代码更容易维护.

编程的过程中, 当需要重构代码的时候, 我往往就是在需要重构的代码的前面和后面,
开始自己的函数提取. 这样, 不自觉地又减低了代码的可维护性.

对外的接口代码, 其上下文是抽象层级较高的概念; 而重构抽取的代码, 往往是底层细节的内容.
如果代码的布局上高层和低层的概念交叉在一起, 对于代码的阅读者来说, 其理解的负担无疑是加重的.

所以把函数分句放在一起, 这样的代码布局, 的确是应该提倡的.

如果函数的一个分句处理的任务还是非常的复杂, 需要把其中的代码提取为单独的函数,
怎么处理呢?

如果把函数分句调用的帮助函数, 统统放在整个函数定义的后面或前面,
当有多个这样的帮助函数的时候, 函数之间的依赖关系就不是那么清晰了.

社区给出的推荐做法是这样的:
保持函数的对外接口不变, 在处理特殊情形的分句中调用一个为这个特殊情况而写一个独立的函数.
这个独立函数, 放在所有的对外接口函数的后面. 然后重构这个独立函数,
并把重构提取的代码放在独立函数的后面. 这样, 从概念在代码中的布局上来看,
总是高层次的概念先于底层概念, 而相关的概念又都保持在相邻的区域内.

例如 Phoenix LiveView 中, 事件的处理的代码, 就可以这样来写:

<!-- livebook:{"force_markdown":true} -->

```elixir
def handler_event('button1_click',_value, stack),do: ...
def handler_event('update_file',value,stack), 
      do: handle_update_file(value,stack)
def handler_event('enven_name', _value,stack),do: ...


defp handle_update_file(value,stack) do
  [v1,v2]=extract(value)
  work_with_v1_and_v2
  ...
end

defp extrac(value) do
   ...
end
```

<!-- livebook:{"break_markdown":true} -->

### Case 与 Cond

在刚开始学习的时候, 遇到需要使用 `case` 或 `cond` 的时候,
我往往犹豫应该使用那一个, 因为我觉得这两个都对应 C 语言的
`swith` 语句.

现在我认为, `case` 语句才对应 C 家族的 `switch`.
`cond` 更加像 C 家族的 `if-else-if` 链.

例如, 下面的 Javascript 代码:

```js
function score_range(leave){
   switch(leave){
     case "F": return [0,59];
     case "E": return [60,65];
     case "D": return [66,74];
     Case "C": return [75,79];
     case "B": return [80, 85];
     case "A": return [86, 89];
     default : return [90,100]
   }
}
```

对应的 Elixir 代码应该为:

<!-- livebook:{"force_markdown":true} -->

```elixir
def score_range(leave) do
  case leave do
    "F" -> [0,  59];
    "E" -> [60, 65];
    "D" -> [66, 74];
    "C" -> [75, 79];
    "B" -> [80, 85];
    "A" -> [86, 89];
    #"A+"
    _v -> [90,100]
  end
end
```

C 语言家族中的 `switch` 和 Elixir 中的 `case` 几乎是逐句对应的.
下面看看 if-else-if 链的代码:

```js
function leave(score){
   if(score < 60)  reurn "F"
   else if(60 <= score && score <= 65) return "E"
   else if(66 <= score && score <= 74) return "D"
   else if(75 <= score && score < 80)  return "C"
   else if(80 <= score && score < 85)  return "B"
   else if(86 <= score && score < 90)  return "A"
   else return "A+"
}
```

对应的 Elixir 应该是:

<!-- livebook:{"force_markdown":true} -->

```elixir
def leave(score) do
  cond do
    score < 60 -> "F"
    60 <= score and score <= 65 -> "E"
    66 <= score and score <= 74 -> "D"
    75 <= score and score < 80 -> "C"
    80 <= score and score < 85 -> "B"
    86 <= score and score < 90 -> "A"
    true -> "A+"
   end
end
```

在 Elixir 中, `case` 和 `cond` 的互换,
要比 C 家族的 `switch` 和 `if-else-if` 语句的互换方便和容易的多.
这也是在 Elixir 中写多分支语句的时候, 我犹豫的一个原因之一.

心理分析学派发现, 找到心理创伤的根源, 往往就可以缓解创伤的症状, 甚至治愈.
实际上一切困惑和神秘的都源于我们还没有很好的理解它们,
如果能够亲手, 以已经了解和掌握的知识和材料, 重新创建不熟悉或神秘的事物,
那么就可以扫清这个思维和心理障碍. 这是我的一点经验体会.
所以, 让我来重新发明轮子, 看看如何用自己熟悉和习惯的概念来实现令我困惑的
`case` 和 `cond`.

观察 `case` 语句的语法, 我发现, 可以把 `case` 语句看作是
Elixir 的立即调用表达式 (IIFE) 的语法糖:
首先把 `case` 语句的 `do-block` 块, 使用 `fn` ... `end` 包裹起来,
就是一个合法的一元匿名函数的定义,
然后立即使用 `case` 的第一个参数来调用这个新创建的匿名函数.
这样就完成了 `case` 语句的工作.

<!-- livebook:{"disable_formatting":true} -->

```elixir
defmodule Case do
  defmacro my_case(v, do: block) do
    iife(block, v)
  end

  defp iife(body, v) do
    {
      {:., [], #This is the `.` in `(fn...end).(v)`
       [
         {:fn, [], body}#This is: `(fn...end)`
       ]},
      [],
      [v] #This is `(v)` in `(fn...end).(v)`.
    }
  end
end
```

```elixir
import Case

fun = fn v ->
  my_case v do
    "A+" -> [90, 100]
    "A" -> [86, 89]
    "B" -> [80, 85]
    "C" -> [70, 79]
    "E" -> [60, 79]
    "F" -> [0, 60]
  end
end

[90, 100] = fun.("A+")
[0, 60] = fun.("F")
# (FunctionClauseError) no function clause matches
fun.("FFF")
```

像上面我们分析的那样, `cond` 的语句可以转化为 `if-else-if` 语句链.
现在我们就以这样的思路来实现我们的 `cond`:

```elixir
defmodule Cond do
  defmacro my_cond(do: block) do
    if Enum.all?(block, &check_syntax/1) do
      block
      |> Enum.map(&elem(&1, 2)) # get `[left,right] from `left -> right`. 
      |> Enum.reverse() #make if-else-if chain more easy
      |> Enum.reduce(make_last_else_part(), &make_if_else/2)
    else
      {:cond, [], [[do: block]]}
    end
  end

  defp check_syntax({:->, _, v}) when is_list(v), do: true
  defp check_syntax(_), do: false

  defp make_if_else([[condtion], then], else_part) do
    {
      :if,
      [context: Elixir, import: Kernel],
      [condtion, [do: then, else: else_part]]
    }
  end

  defp make_last_else_part() do
    {
      :raise,
      [context: Elixir, import: Kernel],
      ["Can not find the true pattern"]
    }
  end
end
```

测试一下:

```elixir
import Cond
a = 7

my_cond do
  a < 7 -> "less than 6"
  a == 7 -> "eq 7"
  a > 7 -> "greate than 7"
end
```

## 乐观控制结构

* Elixir 的 `with`
* Erlang 中的 `maybe`

他们特殊的地方在于, 当模式匹配出错的时候, 这些结构中, 有对应的控制结构,
来处理最后匹配失败的数据.

关于 `with` 和 `maybe` 的更详细的讨论,
见本书的第5章 &Lt;[定制新结构](ch5.new_constructor.livemd)&Gt;.

## 异常处理中的模式匹配

异常处理中的模式匹配的处理是特殊的.

1. `catch` 处理的是函数调用参数列表; 即使如此, 对 `:throw` 类型的错误, 还有语法糖加持.
   参数列表这种语法形式, 只有在函数定义和 `catch` 的时候才是合法的, 其他地方都是不合法的语句,
   所以这种模式匹配是非常独特的.
2. `rescue` 子句中, 模式匹配时只匹配异常名字. 而且对于运行时错误,
   还可以使用 `ErlangError` 来统配的. 而如果要绑定错误对象就必须使用 guard 匹配; 当然了, 还是阉割过的 guard 匹配, 如果和 `case` 或函数定义是的 guard 子句比较的化.

像下面的代码展示的那样, 他们都不是常规的模式匹配.

```elixir
ExUnit.start(auto_run: false)

defmodule MatchTest do
  use ExUnit.Case

  test "ErlangEroor is not same as OtherError" do
    refute ErlangError == FunctionClauseError
  end

  test "catch think throw match two diff pattern" do
    catch_a_value =
      try do
        throw(1)
      catch
        v -> v
      end

    catch_a_tuple =
      try do
        throw(1)
      catch
        :throw, v -> v
      end

    assert catch_a_value == catch_a_tuple
  end

  test "rescue think EralngError match all RuntimeError" do
    a =
      try do
        1 / 0
      rescue
        ArithmeticError -> :ok
        _ -> :try_do_not_know1
      end

    b =
      try do
        1 / 0
      rescue
        ErlangError -> :ok
        _ -> :try_do_not_know2
      end

    assert a == b
  end
end

ExUnit.run()
```

更多的内容见后面的[错误处理章节](./ch8.error_handle.livemd).
