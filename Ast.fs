module Ast

open Type
    
type TExpr =
    | TEFun of TExpr * TExpr
    | TETuple of TExpr * TExpr list
    | TEList of TExpr
    | TEADT of string * TExpr list
    | TEVal of string

type Expr =
    | Lit of Lit
    | Val of string
    | Tuple of Expr * Expr list
    | List of Expr list
    | IfElse of Expr * Expr * Expr
    | CaseList of Expr * string * string * Expr * Expr // 只能匹配list的pattern match
    | App of Expr * Expr
    | Lam of string * Expr
    | Let of string * string list * Expr * Expr // 第二个参数用于元组解构
    | LetRec of string * Expr * Expr
    // 定义抽象数据类型ADT
    // string 是ADT的名
    // string list 是ADT的泛型参数列表
    // (string * TExpr option) list 是ADT的构造器列表
    | ADTDef of string * string list * (string * TExpr option) list * Expr // 定义抽象数据类型
    | Case of Expr * (string * string list * Expr) list // 模式匹配