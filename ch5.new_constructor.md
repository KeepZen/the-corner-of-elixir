# 定制新结构

## 定义自己的控制控制结构

Elixir 是一个特别灵活的语言, 这不但是说, Elixir 本身提供的语法结构非常灵活,
更加重要的是, 如果我们不满意 Elixir 提供的控制结构, 那么可以按照自己的意图,
来定义自己的控制结构.

## 赋值

### 赋值与思维流

确切的说, `=` 叫做匹配操作符, 而不是赋值操作符.
但是变量可以匹配任意的值, 所以实际上, Elixir 中还是用 `=` 来做赋值的操作.

在其他编程语言中, 赋值是一个非常简单的操作, 因为赋值语句非常简短,
基本上一眼就能看清楚赋值的结果是什么.

其他语言中, 块语句不是表达式, 但是在 Elixir 中语句块也是表达式.
当赋值语句的右侧出现的是语句块的时候, 代码序列往往和思维序列不协调.

首先让我来描述一下, 我阅读这样代码时的思维过程.
因为是一个块语句, 也就是说 `=` 的右边需要多个步骤, 多个操作才能完成最后的结果.
阅读代码的时候, 思维自然要跟随代码, 以理解代码的意图.
当多个步骤执行完成后, 在我的思维中, 往往忘记了 `=` 
的左边的表达式是什么了, 因为最后结果和变量之间的距离太远了, 这个距离既是物理上的,
也是心理上的.

而写一个块语句的时候, 在完成整个功能之前, 实际上我不知道最后的结果会是什么,
甚至不知道, 这个块语句就是函数最后的结果, 还是只是计算的中间过程.
所以基本上, 我也是先完成块语句然后再决定是否需要把结果赋值给一个变量的.

当我意识到需要把块语句的结果赋值给一个变量的时候,
这时候光标往往是停留在块语句的结尾处, 要完成赋值语法, 只好移动光标,
这让我有些烦恼.

我更希望写出的代码能和思维流一致, 这样写代码的时候流畅, 阅读的时候也更轻松.

### 设计赋值语句

Elixir 支持宏编程, 这就给我们提供了设计自己的赋值语句提供了可能.
但是 `=` 是一个特殊形式, 所以不能从新定义它, 而只能利用 `=`
提供的功能, 来重新设计新的语法糖.

首先让我来描述清楚, 我想要的语法是什么样的.
```elixir
if exprestion do
  :zero
else
  :is_not_zero
end
|> assign(to: v)
```

这样简单的, 可以使用 Elixir 提供的语法直接转化成为一行的代码:
 `v = if exprestion, do: :zero, else: :is_not_zero`.
这个时候使用 `assign/2` 是没有必要的. 但是当 `if` 语句不能转化成一行的代码时候,
或者当需要把 `case` 或 `cond` 结果赋值为一个变量的时候,
代码序列和思维流之间的阻抗就更加的明显了. 这个时候,
使用 `assign/2` 能让代码易写易读.

假设我们要控制一个机器人, 机器人使用一个 GenServer 表示,
它的状态是 `%{x: x, y: y, diriction: direction}`,
表示机器人所在的位置 `x, y` 和面对的方向
(`:E`, 东方; `:W`, 西方;`:N`, 北方; `:S`, 南方).
当机器人接受到转弯的指令后, 就会按照指令转弯, 从而改变自己的状态.
那么处理右转的代码就是这样:
```elixir
def handler_cast({:turn, :right}, %{direct: direct}=state) do
   direct = case direct do
     :N -> :E
     :E -> :S
     :S -> :W
     :W -> :N
   end

   {:noreply, %{state| direct: dircet}}
end
```

那么在 `assign/2` 的帮助下, 可以把上面的代码重构为:
```elixir
def handler_cast({:turn, :right}, %{direct: direct}=state) do
   case direct do
     :N -> :E
     :E -> :S
     :S -> :W
     :W -> :N
   end
   |> assign(to: direct)

   {:noreply, %{state| direct: dircet}}
end
```

这样的代码整洁了不少吧? 现在让我们来看看, 如何来实现 `assgin/2`.

### 赋值实现

我们需要把 `assgin/2` 定义为一个宏, 这个宏的功能非常的简单, 只是 `=` 的语法糖嘛, 所以可以这样来实现.

```elixir
defmodule Corner.Assign do
  defmacro assign(value, to: pattern) do
    quote do
      unquote(pattern) = unquote(value)
    end
  end
  #...other
end
```

这个实现, 完成了我们期望的工作.

这里 `assign/2` 并不改变直接使用 `=` 的语义代码的语义.
调用 `assign(value, to: pattern)`,
当 `pattern` 和 `value` 不匹配的时候, 依旧会抛出匹配错误,

因为 `assign(value, to: pattern)` 本质上就是 `pattern = value`,
所以任何可以作为匹配操作符 `=` 左值的语法, 都可以作为 `assign/2` 的 `:to` 的值,
毕竟 `assign/2` 仅仅是语法糖.

这样的语法糖, 让代码更简洁, 美观, 如此而已.

`assgin/2` 现在帮助我们减少了代码的序列和思维流之间的阻抗,
但是像上面对 `hanlder_cast/2` 的重构那样,
最后的返回语句不能使用管道操作符 `|>` 连接起来, 这样的代码序列,
反映到思维流上, 就是思维流的打断.

如果 `assign/2` 能帮助我们把思维流也接续起来, 那么写 elixir 代码就更加愉悦了.

能让思维流保持连续的代码, 我认为是这样的:
```elixir
def handler_cast({:trun, :right}, %{direct: direct}=state) do
   case direct do
     :N -> :E
     :E -> :S
     :S -> :W
     :W -> :N
   end
   |> assign(to: direct, do: %{state| direct: direct})
end
```

再假设有现在我们有三个函数, 分别是 `step1:: ()->any`,
`step2:: (any) ->{:ok,t}|{:error,any}` 和 `step3::(t)->any`.
`step1` 的结果作为 `step2` 的输入, `step2` 返回 `{:ok,v}` 时.
`v` 作为 `step3` 输入.
那么无论是:

```elxiir
{:ok, v} = step1()
|> step2()

step3(v)
```
还是:
```elixir
step1()
|> step2()
|> assign(to: {:ok, v})

step3(v)
```

阅读的时候, 思维流都是被打断的.
但如果 `assign` 允许我们写下面的代码, 就能让思维流保持连续.

```elixir
step1()
|> step2()
|> assign(to: {:ok, v}, do: v)
|> step3()
```

也就是说, 为`assign/2` 在添加一个可省略的 `:do` 选项, 就能让思维流接续起来.

这个实现也非常的容易, 当我们完成 `pattern = value` 的赋值后, 再计算 `do: expression`
的值可以了.
```elixir
defmodule Corner.Assign do
  #assing(value, to: pattern)
  defmacro assign(value, to: pattern, do: expression) do
    quote do
      unquote(pattern) = unquote(value)
      unquote(expression)
    end
  end
end
```
有了 `assgin(value,to: pattern, do: block)` 的帮助,
上面的多步骤组合的代码就可以这样来写.
```elixir
defmodule Demo do
  import Corner.Assgin, only: [assign: 2]
  defp step1(), do: 1
  defp step2(v), do: {:ok, v + 1}
  defp step3(v), do: IO.inspect(v, label: "in step3 v")
  
  def do_work() do
    step1()
    |> step2()
    |> IO.inspect(label: "after setp2")
    |> assign(to: {:ok, v}, do: v)
    |> IO.inspect(label: "after assign/2")
    |> step3()
  end
end

Demo.do_work()
#after setp2 : {:ok, 2}
#after assign/2 : 2
#in step3 v: 2
```

在 `assign/2` 的帮助下, 我们写的 Elixir 代码, 几乎也可以做到 free-point 了.

如果赋值之后, 我们需要做非常多的计算, 那么使用 `do: (...)` 就不那么方便,
对这种情况, 可以非常简单的提供一个 `assign(value, [to: pattern] ,do: block)`
宏来解决.
````elixir
defmacro assign(value, [to: pattern], do: expression) do
  quote generated: true do
    unquote(pattern) = unquote(value)
    unquote(expression)
  end
end
````
在导入我们的宏之后, 就可以如此来使用赋值语句了:
````elixir
import Corner.Assign
function()
|> assing(to: {:ok, v}) do
  do_lot_of_work_with(v)
end
````
可以看出, 在管道操作符右侧, 使用 `do-end` 块会使得代码编码的不那么整齐.
所以最终的库中, 我没有提供 `assign/3` 这个宏.

正常的情况下, 使用 `:do` 选项中应该只有一个表达式.
如果发现 `:do` 选项中, 需要两个或更多的表达式才能完成最终的工作,
那么第一种选择是把 `:do` 选项中的语句提取为一个新的函数.
第二种选择是在 `:do` 使用变量,绑定中间结果.
然后在后续的代码中, 使用变量绑定的值, 完成剩余的工作.
```elixir
step0()
|> step1()
|> assgin(to: {:ok, v}, do: v)
|> extract_function_from_do()
|> step2()
|> ...
```
或者
```elixir
step0()
|> assign(to: {:ok, v}, do: tem = do_sample_with(v) )

the_code_use_tems
|> step2()
|> ...
```

## with

如果要选**最不喜欢的语言结构**, Elixir 的 `with` 是我的答案.

我所以不喜欢这个语句结构, 首先是像 `with` 语句中的 `<-` 暗示的那样,
其中的代码序列和逻辑思维流之间存在阻抗. 其次, `with` 结构的代码布局非常的不美观.

例如代码:
```elixir
with                     # with 头部开始
     {:ok, a} <- fun1(), # 产生子代码
     #...                # 更多产生子代码
     {:ok, b} <- fun2(a) # with 头部结束
do                       # with 体开始
     action_with_a_and_b # with 体结束
else                     # 尾部开始
   patter1 -> hanlder_error1
   patter2 -> hanlder_error2 # 尾部结束
end
```
这个 `with` 结构, 像代码中注释的那样, 可以分成 3 部分, 分别是**头**, **体**和**尾**.
通常情况下, `with` 代码就像上面的片段展示的这样, `with` 头往往有多个语句,
而 `with` 体却只有一行或很少的几行代码. 像这样的头部或者参数部分, 
多于体或者正文的语法结构, 除了 `with` 语句外, 我没有见过别的语法有这样的布局.
这样的布局, 给我的感觉是头重脚轻, 极度不协调.
看到这样的代码, 不由自主地, 我的脑海中总是浮现出有染色体缺陷的畸形儿的形象.
所以我非常的不喜欢这个语法结构.

如果不使用 `with` 语句, 等价的代码应该如何写呢? 我认为可以写成这样:
```elixir
try do
  {:ok, a} = fun1()
  {:ok, b} = fun2(a)
  action_with_a_and_b
rescue
  %MatchError{term: term}->
    case term do
      pattern1 -> handler_error1
      pattern2 -> handler_error2
    end
end
```

对于没有 `else` 部分的 `with` 语句, 就更加简单:

```elixir
try do
  {:ok, a} = fun1()
  {:ok, b} = fun2(a)
  action_with_a_and_b
rescue
  %MatchError{term: term} -> term
end
```

而如果语句已经在一个块结构中了, 代码还可以进一步的精简:
```elixir
def fun do
  {:ok, a} = fun1()
  {:ok, b} = fun2(a)
  atction_with_a_and_b
rescue
  %MatchError{term: term} -> term
end
```

所以我认为本质上, `with` 语句只是帮助我们写了 `try` 块.
我认为理想的 `with` 语句应该只要 **体** 和 **尾** 两部分就够了.

### 特殊表达与保留字

在前面的章节中, 已经多次出现 **特殊表单** (Special Forms)这个短语了,
但是特殊表单到底意味着什么呢?

我最初的理解, 特殊表单就像是别的语言中的关键字或者保留字. 这些符号和词汇,
不但有语言规定的意义而且不能用作变量名和函数名, 现在不少的语言,
对保留字的限制有所放宽, 比如 Javascript 中, 保留字虽然还不允许作为变量存在,
但是已经可以作为对象的方法或者字段名了.

在 Elixir 中, 有些特殊表单是没法用 Elixir 的语法来实现的.

比如 `with` 这个特殊表单, 它的特殊就在于其参数个数是不定的,
而 Elixir 中, 无论是函数还是宏的定义, 都不允许不定参数,
所以使用 Elixir 自身的语法是没法定义 `with` 这样的参数个数不定的宏.
但是 `with` 可以作为变量吗? 可以作为我们的函数名吗?
让我们来探索一下.

```elixir
with = 2
IO.inspect(with == 2, label: "with can used as variable")

with {:ok, a} <- {:ok, 1},
     {:ok, b} <- {:ok, a + 2} do
  a + b
end
|> then(&(&1 == 4))
|> IO.inspect(label: "with can still work")

defmodule A do
  def with(a, b), do: a + b
end

A.with(1, 2)
|> then(&(&1 == 1 + 2))
|> IO.inspect(label: "with can use as function name")
```

特殊表居然可以用作变量和函数名. 而且即使用作变量, 也不影响它作为特殊表单的功能.

但是保留字不一样, Elixir 中也有保留字, 虽然非常非常的少.
只有: `true`, `false`, `nil`,`when`, `and`, `or`, `not`,`in`, `fn`, `do`,
`end`,`throw`, `catch`,`rescue`, `after` 和 `else`, 共 16 个.
但是 `with` 不是, 只要不是保留字, 就可以作为变量, 以及函数名, 甚至模块名.

从上面的代码的输出中, 我们可以看出, 实际上 Elixir 对 `with` 几乎没有做限制.

我不满意 `with` 结构, 现在知道, 我们可以使用 `with` 来定义宏.
但是这不是一个好决定.

首先, 使用 `with` 作为宏的名字, 使得定义的宏不能直接导入到客户端的上下文中,
而只能用 `require` 加载宏, 然后带着模块名来使用我们定义的宏,
像上面的代码第 15 行展示的那样, 使用模块前缀来调用模块中的, 以 `with` 命名的宏.
因为特殊表单 (这里是 `with`) 不允许被覆盖, 我们不能通过
`import Kernel.SpecialForms, expect: [with: 1]` 来排除它.
```elixir
defmodule CanNotExceptSpecailForm do
  import Kernel.SpecialForms, except: [with: 1]
  # ... other code not use Kernel.with
end
```
所以, 只要向环境导入特殊表单同名且参数个数一样的函数或宏, 比如 `A.with/1`,
就会引发了编译器的报错.

最后, 即使不是特殊表达, 而是一般的宏, 如果我们自定义的和 `Kernel` 模块中的不兼容的话,
还是不建议使用同名覆盖的策略. 让我们看以下, 假如我们要覆盖 `if/2` 这个宏会发生什么吧.

```elixir
defmodule IF do
  import Kernel, except: [if: 2]

  defmacro __using__(_opt) do
    quote do
      import Kernel, except: [if: 2]
      import IF
    end
  end

  defmacro if(a, b) do
    a && b
  end
end

defmodule ExceptIfButStillUseIt do
  use IF
  a = 1
  if(a, IO.puts("hello"))
  # ... other code use the IF.if
  Kernel.if(a, do: IO.puts(a))
end
```

这里的问题主要是两点:

1. 对于不熟悉我们的模块的人来说, 看到第 19 行的 `if` 语句会让非常的困惑.
2. 在已经导入我们的宏的上下文中, 如 19 行那样, 必须使用 `Kernel.if` 的时候,
   就只能带着 `Kernel` 模块前缀了.

核心模块中提供的宏, 特殊表单和保留字, 编译器对它们的限制, 可以总结为下表.
| 标识符类型 | 举例 | 用作普通标志? | 可以被覆盖? |
|--------|----|----|---|
| 一般标识符 | `if` | 可以 | 是的 |
| 特殊表达 | `with`, `case` `cond` | 可以  | 不可以 |
| 保留字 | `fn`, `do` 等 | 不可以 | 不可以 |

回到 `with` 的话题上, 现在知道, 我们定义的新宏, 不应该命名为 `with`,
那么应该叫什么呢?

Erlang 的新版本 OTP 25 中引入了新的特性 `maybe`[^erlang-maybe].
它实际上和 `with` 解决一样的问题. 因此, 为了向 Erlang 靠拢,
我们以 `maybe` 来命名我们定义的宏.

[^erlang-maybe]: 详细内容见, https://www.erlang.org/doc/reference_manual/expressions.html#maybe

### maybe 的用法

首先, 让我们确定如何使用 `maybe`.
对于使用 `with` 的代码:

```elixir
with                     # with 头部开始
     {:ok, a} <- fun1(), 
     {:ok, b} <- fun2(a) # with 头部结束
do # with 体开始
     action_with_a_and_b #with 体结束
else #尾部开始
   patter1 -> hanlder_error1
   patter2 -> hanlder_error2 # 尾部结束
end
```
和
```elixir
with                   # with 头部开始
     {:ok, a} <- fun1(), 
     {:ok, b} <- fun2(a) # with 头部结束
do # with 体开始
     action_with_a_and_b #with 体结束
end
```

使用 `maybe` 应该重构为:
```elixir
maybe do                     #maybe body begin
  {:ok, a} = fun1()
  {:ok, b} = fun2(a)
  action_with_a_and_b        #maybe body end
else                         #maybe tail start
   patter1 -> hanlder_error1
   patter2 -> hanlder_error2 #maybe tail end
end
```
和
```elixir
maybe do
  {:ok, a} = fun1()
  {:ok, b} = fun2(a)
  action_with_a_and_b
end
```

需要注意的是:

1. 我们的 `maybe` 中使用的是 `=` 而不再是 `<-`, 而且和 Erlang 中的 `maybe` 也不一样,
   我们不用引入新操作符(Erlang 的 `maybe` 结构中, 引入了新的操作符 `?=`).
2. 新的 `maybe` 结构中, 在 `action_with_a_and_b` 对应的代码中,
   如果发生匹配错误, 会直接返回匹配操作符的右值或跳转到 `else` 段落;
   在 `with` 代码中, `action_with_a_and_b` 对应的代码发生匹配错误, 会抛出异常的.
   这是一点不同.

### maybe 的定义

现在让我们定义 `maybe` 宏.

我们的 `maybe` 接受一个参数, `[do: body, else: tail]`,
`maybe` 宏把代码转化成为对应的 `try` 语句块.

```elixir
defmodule Corner.Maybe do
  defmacro maybe(do: body) do
    quote do
      try do
        unquote(body)
      catch
        :error, {:badmatch, v} -> v
      end
    end
  end

  defmacro maybe(do: body, else: tail) do
    quote do
      try do
        unquote(body)
      catch
        :error, {:badmatch, v} ->
          case v do
            unquote(tail)
          end
      end
    end
  end
end
```
要使用 `maybe` 只要导入我们的宏就可以了.

## 可递归的匿名函数

匿名函数, 因为没有名字, 所以不能直接的在其内部调用自己.
计算机科学中, 这个问题, 可以通过不动点组合子来完成[^fixed_point_combinators].
但是老实说, 对于 Y 组合子, 如果不查看文档的话, 我不能写出正确的表达.
对我来说, Y 组合子太绕了.

[^fixed_point_combinators]: 见 Wikipedia[&Lt;不动点组合子&Gt;](https://en.wikipedia.org/wiki/Fixed-point_combinator)词条.

不使用 Y 组合子, 当然也可以完成匿名函数的递归:
```elixir
tem_fun = fn(fun, n,acc) ->
  if n == 0 do
    acc
  else
    fun.(fun,n-1,acc + n)
  end
end
fun = fn(n,acc) ->
  tem_fun.(tem_fun,n,acc)
end
```

这样的代码框架几乎是固定的, 所以可以定义一个宏 `fn!` 来为自动生产这样的框架.
例如, 我想这样来写代码:

```elixir
fn! fun do
  (n, acc) ->
  if n == 0 do
   acc
  else
    fun.(n-1, acc + n)
  end
end
```

`fn!` 和 `fn` 很像, 通用用来定义匿名函数, 但是在 `fn!` 的上下文中,
可以用函数名来完成递归调用. 从这一点上来说, `fn!` 可以看作是 `fn` 的加强版.

### `fn!` 的定义
现在让我们仔细分析 `fn!` 到底应该帮助我们做些什么.
首先, 我们分析 `fn!` 的输入. 第一个参数是表示递归匿名函数名字的变量.
第二个参数是一个 `do-block`, 其中的内容应该一个或多个 `args -> bodys` 表达式.

我们需要把 `do-block` 中的内容转化为一个真正的递归调用函数,
而所作的就是为每个分句都添加一个表示递归函数自身的参数.
当分句中没有对 `name` 函数的调用时, 新添加的第一个参数, 没有用处,
所以应该添加 `_` 前缀, 以避免恼人的编译警告.
虽然这个新添加的变量, 只要不予 `args` 中的变量重复, 叫什么都无所谓,
但是为了逻辑的清晰, 我们还是用 `name` 来命名这个表示递归函数自己的参数.
在这个整整的递归调用函数内容, 如果有对 `name` 函数的调用,
例如 `name.(a,b)` 这样的表达式, 需要转化为 `name.(name, a,b)`.

这是我们的 `fn!` 宏的最困难的工作, 这个工作由 `make_fn/2` 完成.
而 `make_fn/2` 要做的就是处理没有命名函数的的分句.
处理所有分句后, 把这些分句组合成为一个匿名函数. 所以 `make_fn/2` 的代码也非常简单.

````elixir
defmodule Corner.Fn do
  # 其他代码
  defp make_fn(name, body) do
    new_body = Enum.map(body, &clause_handler(name, &1))
    {:fn, [], new_body}
  end
  # 其他代码
end
```
`clause_handler/2`函数完成对每个分句的处理.
它做的工作也非常简单, 检查整个函数分句的函数体的语法树, 并修正其中的递归调用的部分.
如果分句的函数体中有修正, 那么修正后的抽象语法树, 也修正前的不一样,
这样, 我们就能知道, 为这个分句新添加的表示递归函数自己的参数, 用不用添加 `_` 前缀.

向参数列表中添加新参数的工作由 `make_args/2` 完成.
这个工作相对来说比较简单, 只是向列表头部添加一个元素而已.
唯一需要注意地方就是, 需要考虑参数中的哨兵语句.

当新的参数和新的函数体都完成后, 最后只需要把它们重新组合为匿名函数,
就完成了对可递归调用匿名函数的定义.

```elixir
defp clause_handler(name_ast = {atom, _, _}, {:->, meta, [args | body]}) do
  new_body = Macro.postwalk(body, &correct_recursive_call(atom, &1))
  new_args =
    if new_body != body do
      make_args(args, name_ast)
    else
      name_ast = "_#{atom}" |> String.to_atom()
      make_args(args, {name_ast, [], nil})
    end
  {:->, meta, [new_args | new_body]}
end

defp make_args([{:when, meta, args}], fun) do
  [{:when, meta, [fun | args]}]
end
defp make_args(args, fun) do
  [fun | args]
end
````

对递归调用的修正工作由函数 `correct_recursive_call/2` 完成.
假设我们的匿名的递归函数叫做 `fun`, 那么 `correct_recursive_call/2` 做的工作就是,
找到 `fun.(a,b)` 对应的抽象语法树, 修改为 `fun.(fun,a,b)` 对应的抽象语法树.
而其他的语法树, 保持原样.
````elixir
# call is ast of `atom.(...args)`.
# The return is ast of `atom.(atom,...args)`.
defp correct_recursive_call(
       atom,
       call = {{:., _, [{atom, _, _} = fun]}, _, args}
     ) do
  call
  |> Tuple.delete_at(2)
  |> Tuple.append([fun | args])
end
defp correct_recursive_call(_, ast), do: ast
````
最后, `fn!` 函数完成的的工作就非常简单了. 首先检查递归匿名函数的所有的分句,
都有相同个数的参数. 然后创建与递归匿名函数参数个数相同的匿名函数,
在其内部代理已经定义好的可递归的匿名函数.

这里有一点点风险, 最后定义的代理匿名函数的变量是自动生成的.
作为中间变量存储可递归匿名函数的变量, 必须和它们不一样.
为了保证这一点, 我们当然可以自己定义参数生产函数.
这里并没有这样做, 而是使用了一点小技巧, 可以确保用来保存可递归匿名函数的变量绝对不会和
`Macro.generate_arguments/2` 产生的参数重复.
`Macro.generate_arguments/2` 产生的参数, 都是小写字母开头的,
所以只要定义的变量, 以大写字母开头就可以了.
在常规的 Elixir 中, 是不能定义大写字母开头的变量的,
但是在宏定义中, 可以这样做.
```elixir
defmacro fn!(name, do: block) do
  case syntax_check(block) do
    {:ok, arity} ->
      var = {:TEM_fun, [], nil}
      tem_fun = make_fn(name, block)
      params = Macro.generate_arguments(arity, nil)
      quote do
        unquote(name) = fn unquote_splicing(params) ->
          unquote(var) = unquote(tem_fun)
          unquote(var).(unquote(var), unquote_splicing(params))
        end
      end
    :error ->
      raise SyntaxError, "the clauses must have the same arity."
  end
end
```
对递归函数子句参数个数做检查的工作有 `syntax_chexk/1` 完成.
它的工作非常的简单, 如果只有一个子句, 那么无论这个子句有多少参数, 都是合法的.
返回 `{:ok,arity}` 就可以了, 子句多于一个, 那么后续子句的参数个数,
必须和第一个子句的参数个数一样. 这是非常典型对列表的递归检查.
```elixir
defp syntax_check([{:->, _, [args | _]} | others]) do
  args = Ast.get_args(args)
  check_args_length(others, length(args))
end
defp check_args_length([], len) do
  {:ok, len}
end
defp check_args_length([{:->, _, [args | _]} | others], len) do
  args = Ast.get_args(args)
  if len == length(args) do
    check_args_length(others, len)
  else
    :error
  end
end
```
最后, 让我们来检验一下我们劳动成果.

```elixir
import Corner.Fn

fn! sum_from_one_to do
  0 -> 0
  n -> n + sum_from_one_to.(n - 1)
end

sum.(100) # 5050 
```
看起来, 我们成功了.
