# MiniML

A static strongly typed, with a Hindley-Milner type inference **toy** programming language.

supports javascript backend.

一门**静态强类型**，使用**Hindley-Milner**类型推导的ML式的**玩具**编程语言。后端生成javascript。



# 编译流程

```
input
  ↓ string
Lexer.fsl
  ↓ Parser.token
Parser.fsy
  ↓ Ast.fs
Infer.fs
  ↓ ProofTree.fs
JsBackend.fs
  ↓ string
output
```



# 特点

类型系统基于Hindley-Milner，因此代码可以不定义任何类型标注（除了Discriminated Union），全靠类型推导，并且能保证类型安全。

比如`let i = 123 in ...`，可以简单的推导出`i`是`Int`类型。

如果是`let id a = a in ...`，推导出`id`的类型是`forall 'T1. ('T1 -> 'T1)`，是一个多态函数，`'T1`是泛型参数。



所有变量都不可变。



列表和元组，`(1, true, false)`，`[(1, 2); (3, 0); (4, 5)]`，`1 :: [2; 3]`，`[2] ++ []`。



对列表的简单的模式匹配（~~复杂的就不行了~~），

```
case list of
| e :: xs -> ...
| [] -> ...
```



函数和柯里化，

```F#
let second a b = b in
let id = second () in
...
```

推导出的类型是：

```
second: forall 'T4 'T2. ('T2 -> ('T4 -> 'T4))
id:     forall 'T6. ('T6 -> 'T6)
```



lambda，

```F#
let second = fun a -> fun b -> b in
let id = (fun a -> fun b -> b) () in
...
```

这个例子的类型和上边一样。



递归函数，

```F#
let rec fac i =
	if i <= 1
	then 1
	else i * fac (i - 1)
in
fac 3
```

```
fac:    (Int -> Int)
```



Discriminated Union以及对应的模式匹配，

```F#
type Tree<a> =
| Empty
| Node (Tree<a>, a, Tree<a>)
in

let tr = Node (Empty, 1, Node (Empty, 2, Empty)) in

case tr of
| Empty -> ...
| Node (left, v, right) -> ...
```

```
type Tree<'T0>
Empty:  forall 'T0. Tree<'T0>
Node:   forall 'T0. ((Tree<'T0>,'T0,Tree<'T0>) -> Tree<'T0>)
tr:     Tree<Int>
```

```F#
type Test<a, b> =
| A (a -> Option<b>) -> Option<a> -> Option<b>
| B (a -> b) -> [a] -> [b]
| C a -> b -> b
in
...
```

```
type Test<'T0,'T1>
A:      forall 'T1 'T0. ((('T0 -> Option<'T1>) -> (Option<'T0> -> Option<'T1>)) -> Test<'T0,'T1>)
B:      forall 'T1 'T0. ((('T0 -> 'T1) -> (['T0] -> ['T1])) -> Test<'T0,'T1>)
C:      forall 'T1 'T0. (('T0 -> ('T1 -> 'T1)) -> Test<'T0,'T1>)
```



写一段快排试试：

```F#
let rec quicksort list = 
    case list of
    | [] -> []
    | x :: xs ->
    	quicksort (filter (fun i -> i < x) xs)
    	++
    	[x]
    	++
    	quicksort (filter (fun i -> i >= x) xs)
in
let rec readUntilInt tip =
    do print tip in
    case intOptionOfString (readline ()) of
    | Some v -> v
    | None -> readUntilInt tip
in
let size = readUntilInt "input size of list:" in
let input = initList size (fun i -> readUntilInt ("input " @ string (i + 1) @ "th int:")) in
do print (string input) in
let res = quicksort input in
do print (string res) in
()
```





# 功能

## Let in

`let`...`in`是该语言的重要关键字，`let`可以定义变量、函数、递归函数、元组解构。

```F#
let v = 1 in

let id a = a in

let rec fac i =
	if i <= 1
	then 1
	else i * fac (i - 1)
in

let t1, t2 = (1, true) in

()
```

变量只能用`A-Z`，`a-z`，`0-9`，`_`这些字符定义。

`let`是Hindley-Milner中实现多态的关键，直接使用lambda`(fun a -> a) 1`，`a`会被推导为`Int`，只有使用`let`才能实现多态函数`let id = fun a -> a in id 1`。



## do

`do`...`in`就是`let _ = ... in`的语法糖



## 基本类型

基本类型有四种，`Int`、`Real`、`String`、`Unit`。

`Int`是整数，比如`1`，`1024`，`-5`。

`Real`是实数，比如`0.0`，`-77.26556`，`9999.1152`。

`String`是字符串，比如`"565697"`、`""`、`"字符串"`。

`Unit`只有一个值`()`。



## 元组

元组可以用来定义由多个任意类型的值组成的结构。举例`(1, true, "2222", (-23.0, false))`。

可以使用let来解构元组`let a, b = (1, true) in ...`。



## 列表

相比元组，列表中的每个值必须是相同类型的，`[1; 2; 3]`中间用`;`分隔，`[[2; 3]; [4; 5]]`。



## Discriminated Union

```
type Tree<a> =
| Empty
| Node (Tree<a>, a, Tree<a>)
in
...
```

```
type Test<a, b> =
| A (a -> Option<b>) -> Option<a> -> Option<b>
| B (a -> b) -> [a] -> [b]
| C a -> b -> b
in
...
```

其中的`<a,b>`是泛型参数，`a -> b`代表函数类型，`->`是右结合的，`[a]`表示列表类型，`[[a]]`是二维列表，`Option<a>`是“标准库”定义的Discriminated Union。



## 模式匹配

匹配Discriminated Union：

```F#
case tr of
| Empty -> ...
| Node (left, v, right) -> ...
```

也可以不用元组解构，

```F#
case tr of
| Empty -> ...
| Node t -> ...
```



匹配列表：

```F#
case list of
| e :: xs -> ...
| [] -> ...
```



## 运算符

`Int`的运算符

```
+ - * / % > < >= <=
```

`Real`的运算符，（ocaml的优点没学到，糟粕学得倒是一堆一堆的）

```
+. -. *. /. >. <. >=. <=.
```

`String`的运算符

```
@
```

列表的运算符

```
++ ::
```

通用运算符

```
== !=
```



## 标准库

少得可怜

```F#
type Option<a> =
| Some a
| None
in
```

```
neg: Int -> Int

print:    String -> Unit
readline: Unit -> String

string: forall 'T1. 'T1 -> String
int:    Real -> Int
real:   Int -> Real

intOfString:       String -> Int
intOptionOfString: String -> Option<Int>

reverse:  forall 'T1. ['T1] -> ['T1]
filter:   forall 'T1. ('T1. -> Bool) -> ['T1] -> ['T1]
initList: forall 'T1. Int -> (Int -> 'T1) -> ['T1]
```



# 生成JS代码

浏览器按F12粘贴到控制台里就能运行了

```
let $emptyList={isTail:true};let $createList=array=>{var list=$emptyList;for(var i=array.length-1;i>=0;i--){list={value:array[i],tail:list}}return list};let $reverse=list=>{let res=$emptyList;let e=list;while(e!==$emptyList){res=$Cons(e.value,res);e=e.tail}return res};let $Cons=(v,list)=>{return{value:v,tail:list}};let $concat=(list1,list2)=>{list1=$reverse(list1);while(!list1.isTail){list2=$Cons(list1.value,list2);list1=list1.tail}return list2};let $adt=(constructorName)=>{return{cname:constructorName}};let $adtConstructor=constructorName=>value=>{return{cname:constructorName,value:value}};let string=(obj)=>{if(obj instanceof Array){let res='('+string(obj[0]);for(var i=1;i<obj.length;i++){res+=","+string(obj[i])}return res+')'}else if(typeof obj==='object'){if(obj===$emptyList){return'[]'}else if(obj.tail){let res='['+string(obj.value);obj=obj.tail;while(obj!==$emptyList){res+=";"+string(obj.value);obj=obj.tail}return res+']'}else if(obj.cname!==undefined){return obj.cname+':'+string(obj.value)}}return String(obj)};let int=(r)=>~~r;let real=(i)=>i;let neg=n=>-n;let intOptionOfString=(str)=>{let res=parseInt(str);return Number.isNaN(res)?None:Some(res)};let intOfString=(str)=>{let res=parseInt(str);if(Number.isNaN(res))throw new Error('cannot parse `'+str+'` to int');return res};let None=$adt('None');let Some=$adtConstructor('Some');let reverse=$reverse;let filter=(f=>(list=>(()=>{let loop=(f=>(res=>(list=>(()=>{let $list=list;let e=$list.value;let xs=$list.tail;return($list.isTail)?(res):((f(e))?(loop(f)($Cons(e,res))(xs)):(loop(f)(res)(xs)))})())));return reverse(loop(f)($emptyList)(list))})()));let initList=(size)=>(f)=>{let arr=[];for(var i=0;i<size;i++){arr.push(f(i))}return $createList(arr)};
let print = console.log;
let readline = prompt;

(()=>{let quicksort=(list=>(() => {let $list = list;let x=$list.value;let xs=$list.tail;return ($list.isTail) ? ($emptyList) : ($concat(quicksort(filter((i=>(i<x)))(xs)),$concat($createList([x]),quicksort(filter((i=>(i>=x)))(xs))))) ;})());return (()=>{let readUntilInt=(tip=>(()=>{let _=print(tip);return (() => {let $case = intOptionOfString(readline(undefined)); if ($case.cname==='Some') return (v=>v)($case.value);else  if ($case.cname==='None') return readUntilInt(tip);else throw new Error('unmatch pattern in case of');})();})());return (()=>{let size=readUntilInt('input size of list:');return (()=>{let input=initList(size)((i=>readUntilInt((('input '+string((i+1)))+'th int:'))));return (()=>{let _=print(string(input));return (()=>{let res=quicksort(input);return (()=>{let _=print(string(res));return undefined;})();})();})();})();})();})();})()
```

没做cps变换和优化，~~生成的代码质量奇差~~。



# 个人感想

本人大三，这个项目是打算当大四的毕业设计来着（因为要考研，能做先做了），一开始是想用CPS当中间表示然后生成汇编，找了一本《Compiling with Continuations》在啃。看了一段时间后，溜了溜了，整个寒假都不够耗的。然后就打算把重点放在类型推导上，在网上找Hindley-Milner相关内容学习。

整个项目从实验到完成大概做了一个多星期，一开始代码还写得挺规范的，等到开始加元组和列表等功能的时候，代码就开始绿皮化了（~~俺寻思这能运行~~）。有时间整理一下代码（~~看到屎一样的代码不要打我~~）。

很感谢这位大神的代码https://github.com/prakhar1989/type-inference，写的真是简明易懂，还有巨量的注释。一开始整个类型推导部分就是借鉴他的，但是他没有实现Let（HM的重点部分之一），我自己想办法怼了一个Let后，发现没法实现多态函数，于是我开始寻思能不能在调用变量的时候把变量的类型的所有typevar都用新的替换一遍，然后试一下好像是成功了`let id = fun a -> a in id 1`（此时我并没有意识到这样做的严重性），我开始想`(fun id -> id 1)(fun a -> a)`这样好像就不需要Let了，还打算把Let从语法树里删掉，之后被各种各样的推导不出来搞炸心态。

然后我就又开始在github上找各种各样的代码，有开局就一堆Monad Transformer然后lift.lift.lift.lift的看得头晕；又有直接把let变量替换到调用处的，好家伙call by name啊，就是那种`id 1`替换为`(fun a -> a) 1`，确实能用，但是。。。还有用动态类型语言写的不想看。总之找了一天没弄成。

30号那天在外边用手机搜Hindley-Milner，搜到了一篇大神写的文章https://www.zybuluo.com/darwin-yuan/note/424724，讲得很是透彻，读懂了PolyType和MonoType在HM中的作用和Let的意义。一回家马上就开始改推导器，成功实现了Let多态。

不得不说Hindley-Milner真的强，又简单又好用，在实现了它的推导器后，加元组、列表、模式匹配那些功能，都是无脑怼上去的，然后就推导出来了（当然也出过很多bug，主要是推导顺序的问题）。

遇到稍微有点难度是Discriminated Union和对应的模式匹配。原本设计是不用任何类型标记的，但是定义Discriminated Union总得写type是吧，于是把类型标注给补上了（只能使用在Discriminated Union）。一开始打算用Haskell那种能柯里化的类型构造子，但是试了一下发现得弄个kind system，而我搞个type system都弄得鸡飞狗跳的，算了算了，最后选择`type A<a, b>`这种写法。因为没有搞kind，所以这门语言的Monad或者Functor算是寄了。

