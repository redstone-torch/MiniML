open Type
open FSharp.Text.Lexing

let comparisonOperatorType = Poly([1], TFun(T 1, TFun(T 1, TPrim PTBool)))
let intComparsionOperatorType = Mono(TFun(TPrim PTInt, TFun(TPrim PTInt, TPrim PTBool)))

let intBinOperatorType = Mono(TFun(TPrim PTInt, TFun(TPrim PTInt, TPrim PTInt)))

let realBinOperatorType = Mono(TFun(TPrim PTReal, TFun(TPrim PTReal, TPrim PTReal)))
let realComparsionOperatorType = Mono(TFun(TPrim PTReal, TFun(TPrim PTReal, TPrim PTBool)))

let standardLibTypeTable =
    [
        "Option", Poly([1], TADT("Option", [T 1]))
    ]

let standardLibConstructorTable =
    [
        "None", Poly([1], TADT("Option", [T 1]))
        "Some", Poly([1], TFun(T 1, TADT("Option", [T 1])))
    ]

let standardLibLetTable =
    [
        "==", comparisonOperatorType
        "!=", comparisonOperatorType

        "<=", intComparsionOperatorType
        "<",  intComparsionOperatorType
        ">",  intComparsionOperatorType
        ">=", intComparsionOperatorType

        "+", intBinOperatorType
        "-", intBinOperatorType
        "*", intBinOperatorType
        "/", intBinOperatorType
        "%", intBinOperatorType

        "<=.", realComparsionOperatorType
        "<.",  realComparsionOperatorType
        ">.",  realComparsionOperatorType
        ">=.", realComparsionOperatorType
        
        "+.", realBinOperatorType
        "-.", realBinOperatorType
        "*.", realBinOperatorType
        "/.", realBinOperatorType
        
        "neg", Mono(TFun(TPrim PTInt, TPrim PTInt))

        "::", Poly([1], TFun(T 1, TFun(TList(T 1), TList(T 1))))
        "++", Poly([1], TFun(TList(T 1), TFun(TList(T 1), TList(T 1))))

        "@", Poly([1], TFun(TPrim PTString, TFun(TPrim PTString, TPrim PTString)))

        "print", Mono(TFun(TPrim PTString, TPrim PTUnit))
        "readline", Mono(TFun(TPrim PTUnit, TPrim PTString))

        "string", Poly([1], TFun(T 1, TPrim PTString))
        "int", Mono(TFun(TPrim PTReal, TPrim PTInt))
        "real", Mono(TFun(TPrim PTInt, TPrim PTReal))

        "intOfString", Mono(TFun(TPrim PTString, TPrim PTInt))
        "intOptionOfString", Mono(TFun(TPrim PTString, TADT("Option", [TPrim PTInt])))

        "reverse", Poly([1], TFun(TList(T 1), TList(T 1)))
        "filter", Poly([1], TFun(TFun(T 1, TPrim PTBool), TFun(TList(T 1), TList(T 1))))
        "initList", Poly([1], TFun(TPrim PTInt, (TFun(TFun(TPrim PTInt, T 1), TList(T 1)))))
    ] @ standardLibConstructorTable

let env = Map.ofList standardLibLetTable, Map.ofList standardLibTypeTable, Map.ofList standardLibConstructorTable

[<EntryPoint>]
let main args =
    match args with
    | [| file |] ->
        let input = System.IO.File.ReadAllText file
        let lexBuf = LexBuffer<char>.FromString input
        let expr = Parser.program Lexer.token lexBuf
        let proofTree = Infer.infer env expr
        let code = JSBackend.generate proofTree

        ProofTree.printAllLetTypeFromTree proofTree
        printfn "==============================================="
        printfn "%s" code

    | _ -> printfn "argument must be a file (one file only)"
    0