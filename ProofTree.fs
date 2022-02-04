module ProofTree

open Type

type ProofTree =
    | PLit of Lit * Type
    | PVal of string * Type
    | PTuple of ProofTree * ProofTree list * Type
    | PList of ProofTree list * Type
    | PIfElse of ProofTree * ProofTree * ProofTree * Type
    | PCaseList of ProofTree * string * string * ProofTree * ProofTree * Type
    | PApp of ProofTree * ProofTree * Type
    | PLam of string * ProofTree * Type
    | PLet of string * string list * ProofTree * ProofTree * Type
    | PADTDef of Type * ADTConstructors * ProofTree
    | PCase of ProofTree * (string * (string * Type) list * ProofTree) list * Type

let setType (t: Type) (tree: ProofTree) : ProofTree =
    match tree with
    | PLit (a, _) -> PLit (a, t)
    | PVal (a, _) -> PVal(a, t)
    | PTuple (a, b, _) -> PTuple(a, b, t)
    | PList (a, _) -> PList(a, t)
    | PIfElse (a, b, c, _) -> PIfElse(a, b, c, t)
    | PCaseList (a, b, c, d, e, _) -> PCaseList(a, b, c, d, e, t)
    | PApp (a, b, _) -> PApp(a, b, t)
    | PLam (a, b, _) -> PLam(a, b, t)
    | PLet (a, b, c, d, _) -> PLet(a, b, c, d, t)
    | PADTDef _ -> tree
    | PCase (a, b, _) -> PCase(a, b, t)

let rec typeOf (tree: ProofTree) : Type =
    match tree with
    | PLit (_, t)
    | PVal (_, t)
    | PTuple (_, _, t)
    | PList (_, t)
    | PIfElse (_, _, _, t)
    | PCaseList (_, _, _, _, _, t)
    | PApp (_, _, t)
    | PLam (_, _, t)
    | PLet (_, _, _, _, t)
    | PCase (_, _, t)
        -> t
    | PADTDef (_, _, tree1) ->
        typeOf tree1
    

let takeMono (t: Type) =
    match t with
    | Mono t -> t
    | Poly _ -> failwith "t must be a monotype"

let monoTypeOf (tree: ProofTree) : MonoType = takeMono (typeOf tree)

let rec printAllLetTypeFromTree (tree: ProofTree) =
    match tree with
    | PLit _ | PVal _-> ()
    | PIfElse (cond, ifBranch, elseBranch, _) ->
        printAllLetTypeFromTree cond
        printAllLetTypeFromTree ifBranch
        printAllLetTypeFromTree elseBranch
    | PCaseList (list, _, _, someBranch, emptyBranch, _) ->
        printAllLetTypeFromTree list
        printAllLetTypeFromTree someBranch
        printAllLetTypeFromTree emptyBranch
    | PTuple (t1, othersT, _) ->
        printAllLetTypeFromTree t1
        List.iter printAllLetTypeFromTree othersT
    | PList (list, _) ->
        List.iter printAllLetTypeFromTree list
    | PApp (lam, arg, _) ->
        printAllLetTypeFromTree lam
        printAllLetTypeFromTree arg
    | PLam (_, body, _) ->
        printAllLetTypeFromTree body
    | PLet (id, othersId, def, body, _) ->
        match othersId with
        | [] -> if (id <> "_") then printfn "%s:\t%s" id (stringOfType (typeOf def))
        | _ ->
            printf "(%s" id
            List.iter (printf ",%s") othersId
            printfn "):\t%s" (stringOfType (typeOf def))
        printAllLetTypeFromTree def
        printAllLetTypeFromTree body
    | PADTDef (adt, constructors, body) ->
        match adt with
        | Poly (_, t) -> printfn "type %s" (stringOfMonoType t)
        | _ -> ()
        constructors
        |> Map.iter (fun id t -> printfn "%s:\t%s" id (stringOfType t))
        printAllLetTypeFromTree body
    | PCase (case, branches, _) ->
        printAllLetTypeFromTree case
        branches
        |> List.iter (fun (_, _, tr) -> printAllLetTypeFromTree tr)