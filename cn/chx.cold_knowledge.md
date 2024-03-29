# 附录 冷知识

## 函数参数个数

在阅读 Elixir 文档的
≪[类型规范•内建的类型](https://hexdocs.pm/elixir/typespecs.html#built-in-types)≫
小节中, 我发现了一个以前没有注意到的知识: Elixir 中函数参数个数的限制.
> arity() 定义为 `0..255`.

这是说, 我们的函数参数个数最多 255个吗? 要写一个有 255 个变量的函数,
这还真的是个体力活. 但是作为程序员, 对于这种重复性体力劳动,
当然要用编程的方式来完成了.
Elixir 提供了 `Code.compile_string/1` 和 `Code.eval_string/1` 函数,
可以把他们看作是 Elixir 语言为我们提供的编译器和解释器的 API.
我们可以使用这两个函数编译或解释执行对应 Elixir 代码.

通过调整生成的函数参数的个数, 可以找到函数参数个数的上限的设定.

```elixir
defmodule Explore.Arity do
  defp generate_code(arity, name_fun?) do
    params_list =
      1..arity
      |> Enum.map(&"p#{&1}")

    params_src = Enum.join(params_list, ",")
    body_src = Enum.join(params_list, " + ")

    if name_fun? do
      """
      defmodule A do
        def test(#{params_src}) do
          #{body_src}
        end
      end
      """
    else
      "fn #{params_src} -> #{body_src} end"
    end
  end

  @default_opt [fun_named?: false, compiled?: true]
  def check(arity, opt \\ []) do
    opt = Keyword.merge(@default_opt, opt)

    src = generate_code(arity, opt[:fun_named?])

    if opt[:compiled?] do
      Code.compile_string(src)
    else
      Code.eval_string(src)
    end
  rescue
    v ->
      error_tip = if opt[:compiled?], do: "compile error", else: "praser error"
      IO.inspect(v, label: error_tip)
      :bad
  else
    v ->
      IO.inspect(v, label: "check done")
      :ok
  end
end
```

让我们用单元测试功能来完成我们探索. 用代码, 能更清楚地表达我的意图.

```elixir
ExUnit.start(auto_run: false)

defmodule ArityTest do
  use ExUnit.Case, async: true
  alias Explore.Arity

  test "the uplimit of arity for anonymous function in complier " do
    assert Arity.check(255) == :ok
    assert Arity.check(256) == :bad
    IO.puts("")
  end

  test "the uplimit of arity for named function in complier " do
    assert Arity.check(255, fun_named?: true) == :ok
    assert 1..255 |> Enum.sum() == apply(A, :test, Enum.to_list(1..255))

    assert Arity.check(256, fun_named?: true) == :ok
    assert_raise UndefinedFunctionError, fn -> apply(A, :test, Enum.to_list(1..256)) end
    IO.puts("")
  end

  test "the uplimit of arity for anonymous function in parser" do
    assert Arity.check(20, compiled?: false) == :ok
    assert Arity.check(21, compiled?: false) == :bad
    IO.puts("")
  end

  test "the uplimit of arity for named function in parser" do
    assert Arity.check(20, fun_named?: true, compiled?: false) == :ok
    assert apply(A, :test, 1..20 |> Enum.to_list()) == 1..20 |> Enum.sum()

    assert Arity.check(21, fun_named?: true, compiled?: false) == :ok
    assert_raise UndefinedFunctionError, fn -> A.test(Enum.to_list(1..21)) end
    IO.puts("")
  end
end

ExUnit.run()
```

通过上面的实验, 可以得出以下结论:

* 在编译器中
  1. 对于匿名函数, 参数个数多于 255 个, 编译器报 UndefineFuntionError
  2. 对于命名函数, 参数个数多于 255 个, 编译器并不报错, 但是调用这个函数,
     运行时会产生 UndefineFuntionError
* 在解析器中
  1. 对于匿名函数, 参数个数多于 20 个, 解释器就会报 UndefinedFunctionError
  2. 对于命名函数, 参数个数多于 20 个, 解释器不会报错, 但是调用这个函数,
     运行时会产生 UndefineFunctionError

## 原子

1. BEAM 中原子个数上限默认为: 1048577.
2. 原子的内存长度是固定的. 原子占用系统一个字 (word) 长度的内存.
3. 原子内容最长可以有 255 个字符.

现在有提议对原子也做垃圾收集, 也许以后的 BEAM 实现, 原子个数上限就会被解除,
那么对于嵌入式的系统, 用原子来代替比较长的字符串可以达到节省内存的目的.

```elixir
ExUnit.start(auto_run: false)

defmodule AtomLimitTest do
  use ExUnit.Case, async: true

  test "255 byte long atom is ok" do
    assert 1..255
           |> Enum.map(fn _ -> "1" end)
           |> Enum.join("")
           |> String.to_atom()
  end

  test "256 byte long auto is up the atom limit" do
    assert_raise SystemLimitError, fn ->
      1..256
      |> Enum.map(fn _ -> "2" end)
      |> Enum.join("")
      |> String.to_atom()
    end
  end
end

ExUnit.run()
```

调用 `:eralng.memory(:atom_used)` 可以看出当前我们的系统中, 原子使用的内存.

## 变量名的限制

变量名实际上也是原子, 所以变量名接受原子的限制, 也就是说变量名不能长于 255 个字符,
但是实际上, 系统对变量还有一些限制, 在 Elixir 中表示变量的原子有统一的格式:
`_{var_name}@1`. 所以, 变量名只能由 255-3 = 252 个字节长度.

我们可以通过下面的代码来得出这个结论.

```elixir
create_var = fn l ->
  1..l
  |> Enum.map(fn _a -> "l" end)
  |> Enum.join("")
end

var = create_var.(252)
assign = "#{var} = 1"
{1, _} = Code.eval_string(assign)
IO.puts("variable length equal 252 is ok")
var = create_var.(253)
assign = "#{var} = 1"
Code.eval_string(assign)
```

## 函数中定义模块

可以在`def/2` 上下文中定义模块:

```elixir
defmodule EmbedModuleToFun do
  def createFun() do
    defmodule :_ do
      def hello(), do: IO.puts("I am in #{__MODULE__}, hello!")
    end
    |> elem(1)
  end
end

m = EmbedModuleToFun.createFun()
m.hello()
```

这让我想起来 Java 的匿名类. Java 的早期版本中, 不支持匿名函数类型,
所以在当初要在 Java 中模拟函数式编程, 定义高阶函数的时候就不得不使用接口.
使用这些高级函数的时候, 往往用一个匿名类来实现相应的接口.

我们这里使用的方法, 和当年 Java 中使用的方法是一样的.

但是使用这个方法的时候, 还有需要注意的地方.
首先本质上, 这个方法是在运行时动态编译了新模块.
所以每次调用 `createFun()` 都会产生新的模块,
这就意味着: 1) 效率下降, 2) 如果需要在多个函数中返回模块,
必须留意为函数返回的模块取不同的名字, 以防止名称冲突.

我们可以写一个宏来解决这两个问题.

```elixir
defmodule Nest do
  import Kernel, except: [def: 2]

  defmacro def(call, exp) do
    if __CALLER__.function == nil do
      raise "Nest.def must cal in def/2"
    end

    {name, args} = name_and_args(call)
    str = "#{__CALLER__.module}.nest_#{name}_#{length(args)}"
    module_name = String.to_atom(str)
    make_module(module_name, call, exp)
  end

  defp name_and_args({:when, _, [{name, _, args}, _]}), do: {name, args}
  defp name_and_args({name, _, args}), do: {name, args}

  defp make_module(module_name, call, exp) do
    quote do
      if :code.is_loaded(unquote(module_name)) do
        unquote(module_name)
      else
        defmodule unquote(module_name) do
          IO.puts("Compile #{unquote(module_name)}")
          def unquote(call), unquote(exp)
        end
        |> elem(1)
      end
    end
  end
end
```

当我们在 LiveBook 中运行下面的测试代码的时候, 如果是第一次运行,
那么第 10 行的的输出之前, 控制台中应该先输出, 编译内部模块的提示:
"Compile Elixir.TestNest.nest_hello_2",
但是第 11 行再次调用 `TestNest.hello()` 的时候, 不再输出模块编译的提示了,
这说明的确不是每次调用 `TestNest.hello()` 都创建新的模块.

```elixir
defmodule TestNest do
  require Nest

  def hello() do
    Nest.def(hello(a, b), do: a + b)
  end
end

3 = TestNest.hello().hello(1, 2)
IO.puts("-----------")
3 = TestNest.hello().hello(1, 2)
```

在 LiveBook 中, 一旦上面的代码计算过一次此后, 再次运行的话,
因为内部模块已经被加载到系统中了, 所以控制台中, 只会有第 10 行的输出,
而不能再看到内部模块的编译提示.

这让我意识到, Erlang 中不是只有进程可以保存状态,
系统内模块的状态结合动态编译, 也可以用来表示状态.
当然了这样做有非常大的效率成本. 但作为理论探索, 不失为一个有趣的话题.

## 进程之外的状态保持

在 Erlang 和 Elixir 中, 最常用的保持状态的方法是启用一个服务进程.
但是 Erlang 的运行时系统, 自身是有状态的.
Erlang 允许在运行时, 动态生成代码.
运行时动态生成的代码改变了 Erlang 运行时的模块的状态.

如果我们把状态信息, 直接保存到模块的元数据中, 在需要改变状态的时候,
重新产生模块, 且以新的状态来设置新产生的模块, 那么外部看来,
这个模块就像一个保有状态的对象一样.

```elixir
defmodule Corner.State do
  def create(kv \\ []) do
    if Keyword.keyword?(kv) do
      now = NaiveDateTime.utc_now() |> to_string() |> String.to_atom()
      module = Module.concat(__MODULE__, now)
      make_nest_module(kv, module)
    else
      raise "Expect a keyword, but get :#{inspect(kv)}"
    end
  end

  defmacro delete(module) do
    if :code.delete(module) do
      quote do
        var!(unquote(module), __CALLER__) = nil
        true
      end
    else
      false
    end
  end

  def set(module, k, v) do
    kv =
      module.module_info(:attributes)
      |> Enum.filter(&(elem(&1, 0) != :vsn))
      |> Enum.map(fn {k, [v]} -> {k, v} end)
      |> Keyword.merge([{k, v}])

    make_nest_module(kv, module)
  end

  defp make_nest_module(kv, module) do
    blocks = make_geter_and_seters(module, kv, [])

    quote do
      defmodule unquote(module) do
        unquote_splicing(blocks)
        def delete(), do: :code.delete(unquote(module))
      end
    end
    |> Code.eval_quoted()
    |> elem(0)
    |> elem(1)
  end

  defp make_geter_and_seters(_module, [], acc), do: acc

  defp make_geter_and_seters(module, [{k, v} | rest], acc) do
    attr = make_attr(k, v)
    geter = quote do: def(unquote(k)(), do: unquote(v))

    seter =
      quote do
        def unquote(k)(v),
          do: Corner.State.set(unquote(module), unquote(k), v)
      end

    make_geter_and_seters(module, rest, [attr, geter, seter | acc])
  end

  defp make_attr(k, v) do
    [
      quote do
        Module.register_attribute(__MODULE__, unquote(k), persist: true)
      end,
      {:@, [context: Elixir, import: Kernel], [{k, [context: Elixir], [v]}]}
    ]
  end
end
```

我们可以使用 `h = Corner.State.create([x: 1])` 来创建一个状态.
并使用 `s.x` 读取 `x` 的值, `s.x(new_value)`
更新 `x` 的值.

```elixir
import Corner.State
s = create(x: 2, y: 0)
s.x |> IO.inspect(label: :x)
s.y |> IO.inspect(label: :y)
s.x(0)
s.x |> IO.inspect(label: :x)
delete(s)
s |> IO.inspect()
```

## 超时

Elixir 的 `receive` 结构中支持 `after` 子句来处理超时问题. `after_sent`
也可以在指定的时间后向进程发送消息.

这些功能背后使用的都是相同的机制, Elixir 中整数是可以无限大的,
但是表示超时的参数却不能这样, 而是有限制的.
在 Elixir 中, 如果是一个正整数, 那么这个正整数,
必须小于等于 32 位可表示的最大无符号整数,
也就是 $2^{33}-1$ (或十六进制表示: 0xFFFF_FFFF).
让我们实验一下:

```elixir
receive do
  a -> IO.inspect(a, label: "a")
after
  0xFFFF_FFFF + 1 -> IO.put("timeout")
end
```

## 远程调用和本地调用

在模块外, 调用一个模块的函数, 实际上使用的都是远程调用.
但是在 `defmodule/2` 的上下文中, 私有函数和公开函数都可以使用本地调用的语法来调用.

原来我一直以为, 在 `defmodule/2` 上下文中, 对模块内的函数, 使用远程调用的语法没有意义.
但是这是错误的. 当代码发生热加载的时候, 在 `defmodule/2` 上下文中,
远程调用和本地调用之间的差别就显示出来了.

Erlang 中热加载是这样实现的:

系统会为每个模块保持两个最新的版本. 假设有两个进程P1, P2; P1 运行的是第一个版本的代码,
P2 运行的是第二个版本的代码. 当模块的第 3 个版本被加载到系统后, 第一个版本的代码,
就会从系统中被删除掉, 这是, 如果有进程还在运行第一个版本的代码,
就会因为找不到对应的模块, 而被杀死.

如果被杀死的 P1 运行在监管树下, 那么监控进程会创建新的进程 P3 代替死亡的 P1,
这时 P3 中运行的是第三个版本的代码. 至此, 和远程函数调用还没有关系.

这样的热更新, 导致了进程 P1 退出, 所以还是有风险的.

如果代码更新后, 还在运行的进程可以自动的切换到最新的版本的代码中,
那么热更新的风险就更小了.

要达到这种效果, 在服务器进程的循环函数中, 最后对自己的递归调用, 就必须使用远程调用的语法.
远程调用语法, 调用的都是模块的最新版本的代码.

下面让我们用代码来实验一下.

首先, 定义第一个版本. 在`loop/0` 递归调用自己的时候,
使用的是本地函数调用的语法:`loop()`.
模块编译完成后, 立刻启东进程 p1 运行这个版本.

```elixir
defmodule RemoteAndLocalCall do
  def loop() do
    receive do
      :ok ->
        IO.puts("#{inspect(self())} version1")
        Process.send_after(self(), :ok, 100)
        loop()

      :stop ->
        IO.puts("stop version1")
    end
  end
end

p1 =
  spawn(&RemoteAndLocalCall.loop/0)
  |> IO.inspect(label: "p1")
```

然后我们定义第二个版本, 这里, `loop/0` 对自己递归调用的时候,
使用的是远程函数调用的语法: `__MODULE__.loop()`.
模块编译后, 我们启动进程 p2 来运行我们的代码. 这时 p1 和 p2 都在等待消息进来.
所以, 通过向他们分别发送一个 `:ok` 消息来启动这两个进程的循环.

这时控制台中会交替的输出 "PID{} version1", "PID{} version2".

```elixir
defmodule RemoteAndLocalCall do
  def loop() do
    receive do
      :ok ->
        IO.puts("#{inspect(self())} version2")
        Process.send_after(self(), :ok, 100)
        __MODULE__.loop()

      :stop ->
        IO.puts("stop version2")
    end
  end
end

p2 =
  spawn(&RemoteAndLocalCall.loop/0)
  |> IO.inspect(label: "p2")

Process.alive?(p1) |> IO.inspect(label: "Is v1 alive?")

for p <- [p1, p2] do
  send(p, :ok)
end
```

最后, 定义第三个版本, 模块编译通过后会发现:

1. 进程 p1 死亡了.
2. 进程 p2 还活着, 但是其中运行的代码更新了.
   控制台输出变成了 `PID{} version3`.

```elixir
defmodule RemoteAndLocalCall do
  def loop() do
    receive do
      :ok ->
        IO.puts("#{inspect(self)} version3")
        Process.send_after(self, :ok, 100)
        __MODULE__.loop()

      :stop ->
        IO.puts("stop version3")
    end
  end
end

Process.alive?(p1) |> IO.inspect(label: "Is v1 live?")
Process.alive?(p2) |> IO.inspect(label: "Is v2 live?")
IO.inspect(p2, label: "P2")
```

最后运行下面的代码, 退出进程 p2.

```elixir
send(p2, :stop)
```
