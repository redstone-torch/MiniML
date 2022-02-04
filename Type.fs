module Type

type PrimitiveType =
    | PTInt
    | PTBool
    | PTUnit
    | PTString
    | PTReal

type TypeVar = int

type MonoType =
    | T of TypeVar
    | TPrim of PrimitiveType
    | TFun of MonoType * MonoType
    | TList of MonoType
    | TTuple of MonoType * MonoType list // 因为空元组()不是元组 这样写可以强制让TTuple不空 (t1, t2, t3, ...) 表示为 t1, [t2; t3; ...]
    | TADT of string * MonoType list // 第二部分是参数列表 用于泛型ADT的确定化

type Type =
    | Mono of MonoType
    | Poly of TypeVar list * MonoType

(*
    每一个Contructor的Type满足如下要求
    要么是一个(Mono/Poly) TADT  要么是一个单参数函数 (Mono/Poly) (? -> TADT)
    不能是多参数函数 (Mono/Poly) (? -> ? -> TADT) 也不能是其他类型
*)
type ADTConstructors = Map<string, Type>

type Lit =
    | LUnit
    | LInt of int
    | LBool of bool
    | LString of string
    | LReal of double

let stringOfPrimtiveType (pt: PrimitiveType) =
    match pt with
    | PTUnit -> "Unit"
    | PTInt -> "Int"
    | PTBool -> "Bool"
    | PTString -> "String"
    | PTReal -> "Real"

let rec stringOfMonoType (m: MonoType) =
    match m with
    | T v -> "'T" + string v
    | TPrim pt -> stringOfPrimtiveType pt
    | TFun (m1, m2) -> "(" + stringOfMonoType m1 + " -> " + stringOfMonoType m2 + ")"
    | TList m -> "[" + stringOfMonoType m +  "]"
    | TTuple (m, othersM) ->
        "("
        + stringOfMonoType m
        + List.fold (fun s m -> s + "," + stringOfMonoType m) "" othersM
        + ")"
    | TADT (name, list) ->
        match list with
        | [] -> name
        | e :: xs ->
            name
            + "<"
            + stringOfMonoType e
            + List.fold (fun s m -> s + "," + stringOfMonoType m) "" xs
            + ">"
            
        
let stringOfType (t: Type) =
    match t with
    | Poly (genericList, m) ->
        List.fold (fun s e -> s + " 'T" + string e) "forall" genericList
        + ". "
        + stringOfMonoType m
    | Mono m ->  stringOfMonoType m

let primtiveTypeOfLit (lit: Lit) =
    match lit with
    | LUnit -> PTUnit
    | LInt _ -> PTInt
    | LBool _ -> PTBool
    | LString _ -> PTString
    | LReal _ -> PTReal

let monoTypeOfLit (lit: Lit) = TPrim (primtiveTypeOfLit lit)

let typeOfLit (lit: Lit) = Mono (monoTypeOfLit lit)