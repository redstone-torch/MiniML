module Infer

open Ast
open Type
open ProofTree

type TypeTable = Map<string, Type>
type ConstructorTable = ADTConstructors
type LetTable = Map<string, Type>
type Enviorment = LetTable * TypeTable * ConstructorTable
type Constraint = MonoType * MonoType
type Substitution = TypeVar * MonoType

let mutable private typeVarCounter = 0

let newTypeVar () : TypeVar =
    let v = typeVarCounter
    typeVarCounter <- v + 1
    v

let newT () = T(newTypeVar())

let collectTypeVar (mono: MonoType) : TypeVar list =

    let rec loop (mono: MonoType) (result: TypeVar list) : TypeVar list =
        match mono with
        | T typeVar -> typeVar :: result
        | TPrim _ -> result
        | TFun (m1, m2) -> result |> loop m1 |> loop m2
        | TList m -> result |> loop m
        | TTuple (firstM, otherM) ->
            let result = result |> loop firstM
            List.fold (fun res t -> loop t res) result otherM
        | TADT (_, list) -> List.fold (fun res m -> loop m res) result list

    loop mono []
    
let getFreeVars (t: Type) : TypeVar list =
    match t with
    | Mono m -> collectTypeVar m
    | Poly (genericVarList, m) ->
        let typeVarList = collectTypeVar m
        let freeTypeVarList = typeVarList |> List.except genericVarList
        freeTypeVarList

let getFreeVarsFromEnv ((typeTable, _, _): Enviorment) : TypeVar list =
    typeTable.Values
    |> Seq.collect getFreeVars
    |> Seq.toList

let addLetToEnv (id: string) (t: Type) ((typeTable, adtTable, constructorTable): Enviorment) : Enviorment =
    Map.add id t typeTable, adtTable,  constructorTable

let addTypeToEnv (name: string) (t: Type, constructors: ADTConstructors) ((typeTable, adtTable, constructorTable): Enviorment) : Enviorment =
    
    let merge m1 m2 =
        Map.fold (fun m k v -> Map.add k v m) m1 m2

    typeTable, Map.add name t adtTable, merge constructors constructorTable

let tryFindLetType (id: string) ((letType, _, _): Enviorment) : Type option =
    Map.tryFind id letType

let tryFindType (id: string) ((_, typeTable, _): Enviorment) : Type option =
    Map.tryFind id typeTable

let tryFindConstructor (id: string) ((_, _, constructorTable): Enviorment) : Type option =
    Map.tryFind id constructorTable

let rec substitute (u: MonoType) (x: TypeVar) (m: MonoType) : MonoType =
    match m with
    | T v -> if v = x then u else m
    | TPrim _ -> m
    | TFun(m1, m2) -> TFun(substitute u x m1, substitute u x m2)
    | TList m -> TList (substitute u x m)
    | TTuple (first, other) -> TTuple(substitute u x first, List.map (substitute u x) other)
    | TADT (typeName, typeVars) -> TADT (typeName, List.map (substitute u x) typeVars)
    
let apply (subs: Substitution list) (t: MonoType) : MonoType =
    List.foldBack (fun (x, u) t -> substitute u x t) subs t

let gen (env: Enviorment) (t: Type) : Type =
    match t with
    | Poly _ -> t
    | Mono m ->
        let genericVarList =
            getFreeVars t
            |> List.except (getFreeVarsFromEnv env)
            |> List.distinct
        match genericVarList with
        | [] -> Mono m
        | list -> Poly (list, m)

let inst (t: Type) : MonoType =
    match t with
    | Mono m -> m
    | Poly (genericVarList, m) ->
        let replaceSubs =
            genericVarList
            |> List.map (fun old -> old, newT())
        apply replaceSubs m

let createADT (name: string) (genList: string list) (constructors: (string * TExpr option) list) (env: Enviorment) =
                
    let rec createType (genMap: Map<string, TypeVar>) (env: Enviorment) (texpr: TExpr) : MonoType =
        match texpr with
        | TEVal v ->
            if Map.containsKey v genMap then
                T (Map.find v genMap)
            else
                match tryFindType v env with
                | Some t -> inst t
                | None -> raise (failwithf "cannot find type `%s`" v)
        | TETuple (t1, othersT) ->
            TTuple(createType genMap env t1, List.map (createType genMap env) othersT)
        | TEFun (a, b) ->
            TFun(createType genMap env a, createType genMap env b)
        | TEList a ->
            TList(createType genMap env a)
        | TEADT (adt, params') ->
            match tryFindType adt env with
            | Some v ->
                match v with
                | Mono (TADT _) -> TADT(adt, [])
                | Poly (_, TADT (_, genList)) when List.length genList = List.length params' ->
                    TADT(adt, List.map (createType genMap env) params')
                | _ -> raise (failwith "type parameter error")
            | None -> raise (failwithf "type '%s' not found" adt)

    let rec createGenericMap (name: string) (map: Map<string, TypeVar>) (genList: string list) =
        match genList with
        | [] -> map
        | g :: xs ->
            if Map.containsKey g map then
                raise (failwithf "duplicate generic parameter `%s` in type `%s`" g name)
            else
                createGenericMap name (Map.add g (newTypeVar()) map) xs

    let rec createConstructorMap
            (adt: MonoType)
            (genMap: Map<string, TypeVar>)
            (conMap: Map<string, Type>)
            (env: Enviorment)
            (cons: (string * TExpr option) list)
            =
        match cons with
        | [] -> conMap
        | (consName, None) :: xs ->
            if Map.containsKey consName conMap then
                raise (failwithf "duplicate define type constructor `%s`" consName)
            else
                let genType = gen env (Mono adt)
                let conMap = (Map.add consName genType conMap)
                createConstructorMap adt genMap conMap env xs
        | (consName, Some texpr) :: xs ->
            if Map.containsKey consName conMap then
                raise (failwithf "duplicate define type constructor `%s`" consName)
            else
                let genType = gen env (Mono(TFun(createType genMap env texpr, adt)))
                let conMap = (Map.add consName genType conMap)
                createConstructorMap adt genMap conMap env xs
    
    

    let genericMap = createGenericMap name (Map.empty) genList

    let adtType = TADT(name, genericMap.Values |> Seq.map T |> Seq.toList)

    let genADTType = gen env (Mono adtType)

    let env = addTypeToEnv name (genADTType, Map.empty) env

    let constructorMap = createConstructorMap adtType genericMap Map.empty env constructors
    

    genADTType, constructorMap

let rec applyTree (subs: Substitution list) (tree: ProofTree) : ProofTree =
    match tree with
    | PLit _ | PADTDef _ -> tree

    | PVal (id, t) -> PVal(id, Mono (apply subs (takeMono t)))

    | PTuple (first, last, t) ->
        PTuple(applyTree subs first, List.map (applyTree subs) last, Mono (apply subs (takeMono t)))

    | PList (list, t) ->
        PList (List.map (applyTree subs) list, Mono (apply subs (takeMono t)))

    | PIfElse (cond, ifBranch, elseBranch, t) ->
        PIfElse(applyTree subs cond, applyTree subs ifBranch, applyTree subs elseBranch, Mono (apply subs (takeMono t)))

    | PCaseList (list, e, xs, someBranch, emptyBrach, t) ->
        PCaseList(applyTree subs list, e, xs, applyTree subs someBranch, applyTree subs emptyBrach, Mono (apply subs (takeMono t)))

    | PLam (param, body, t) ->
        PLam(param, applyTree subs body, Mono (apply subs (takeMono t)))

    | PApp (lam, arg, t) ->
        PApp(applyTree subs lam, applyTree subs arg, Mono (apply subs (takeMono t)))

    | PLet (id, othersId, def, body, t) ->
        match typeOf def with
        | Poly _ -> PLet(id, othersId, def, applyTree subs body, Mono (apply subs (takeMono t)))
        | Mono _ -> PLet(id, othersId, applyTree subs def, applyTree subs body, Mono (apply subs (takeMono t)))

    | PCase (case, branches, t) ->
        PCase(
            applyTree subs case,
            branches |> List.map (fun (a, list, c) ->
                a,
                List.map (fun (a, b) -> a, Mono (apply subs (takeMono b))) list,
                applyTree subs c),
            Mono (apply subs (takeMono t)))

let rec unify (cons: Constraint list) : Substitution list =
    match cons with
    | [] -> []
    | (x, y) :: xs ->
        let t1 = unify xs
        let t2 = unify_one (apply t1 x) (apply t1 y)
        t2 @ t1

and unify_one (t1: MonoType) (t2: MonoType) : Substitution list =
    match t1, t2 with
    | TPrim pt1, TPrim pt2 when pt1 = pt2 -> []
    | T x, z | z, T x -> [(x, z)]
    | TFun (a, b), TFun(x, y) -> unify [(a, x); (b, y)]
    | TList m1, TList m2 -> unify [(m1, m2)]
    | TTuple (m1, ms1), TTuple (m2, ms2) -> unify ((m1, m2) :: List.zip ms1 ms2)
    | TADT (name1, args1), TADT (name2, args2) when name1 = name2 ->
        List.zip args1 args2
        |> List.collect (fun (a, b) -> unify [(a, b)])
    | _ -> raise (failwithf "mismatched types '%A' and '%A'" t1 t2)

let rec genProofTreeAndCollectConstraint (env: Enviorment) (expr: Expr) : ProofTree * Constraint list =
    match expr with
    | Lit lit -> PLit (lit, typeOfLit lit), []

    | Tuple (first, others) ->

        let firstTree, firstCons = genProofTreeAndCollectConstraint env first

        let othersTreeList, othersConsList =
            others
            |> List.map (genProofTreeAndCollectConstraint env)
            |> List.unzip
        
        let cons = firstCons @ List.collect id othersConsList

        let tupleType = Mono(TTuple(monoTypeOf firstTree, List.map monoTypeOf othersTreeList))

        PTuple(firstTree, othersTreeList, tupleType), cons

    | List list ->
        let listType = TList(newT())
        let listTree, cons =
            List.foldBack (fun e (tree, cons) ->
                let eTree, eCons = genProofTreeAndCollectConstraint env e
                match tree with
                | PList (list, t) -> PList(eTree :: list, t), (takeMono t, TList (monoTypeOf eTree)) :: eCons @ cons
                | _ -> raise (failwith "impossible")
            ) list (PList ([], Mono listType), [])
        listTree, cons
    
    | Val id ->
        let t =
            match tryFindLetType id env with
            | Some v -> v
            | None -> raise (failwithf "symbol not found '%s'" id)
        //let t' = newT()
        //PVal(id, Mono t'), [inst t, t']
        PVal(id, Mono (inst t)), []

    | IfElse (cond, ifBranch, elseBranch) ->
        let ifElseType = newT()
        let condTree, condCons = genProofTreeAndCollectConstraint env cond
        let condCons = (TPrim PTBool, monoTypeOf condTree) :: condCons
        let ifTree, ifCons = genProofTreeAndCollectConstraint env ifBranch
        let elseTree, elseCons = genProofTreeAndCollectConstraint env elseBranch
        let cons = (monoTypeOf ifTree, monoTypeOf elseTree) :: elseCons @ ifCons @ condCons
        let cons = (ifElseType, monoTypeOf ifTree) :: cons
        PIfElse(condTree, ifTree, elseTree, Mono ifElseType), cons

    | CaseList (list, e, xs, someBranch, emptyBrach) ->
        let caseListType = newT()

        let elementType = newT()
        let listTree, listCons = genProofTreeAndCollectConstraint env list
        let listType = monoTypeOf listTree
        let listCons = (TList(elementType), monoTypeOf listTree) :: listCons

        let eType = elementType
        let xsType = listType
        let env1 = env |> addLetToEnv e (Mono eType) |> addLetToEnv xs (Mono xsType)
        let someTree, someCons = genProofTreeAndCollectConstraint env1 someBranch
        let emptyTree, emptyCons = genProofTreeAndCollectConstraint env emptyBrach

        let cons = (monoTypeOf someTree, monoTypeOf emptyTree) :: emptyCons @ someCons @ listCons
        let cons = (caseListType, monoTypeOf someTree) :: cons
        PCaseList(listTree, e, xs, someTree, emptyTree, Mono caseListType), cons

    | App (lam, arg) ->
        let lamTree, cons1 = genProofTreeAndCollectConstraint env lam
        let lamMono = monoTypeOf lamTree
        let argTree, cons2 = genProofTreeAndCollectConstraint env arg
        let argMono = monoTypeOf argTree
        let appMono = newT()
        let cons = (TFun(argMono, appMono), lamMono) :: cons2 @ cons1
        PApp(lamTree, argTree, Mono appMono), cons

    | Lam (param, body) ->
        let lamMono = newT()
        let parmMono = newT()
        let env = addLetToEnv param (Mono parmMono) env
        let bodyTree, cons = genProofTreeAndCollectConstraint env body
        let bodyMonoType = monoTypeOf bodyTree
        let cons = (TFun (parmMono, bodyMonoType), lamMono) :: cons
        PLam(param, bodyTree, Mono lamMono), cons

    | Let (id, othersId, def, body) ->
        let letType = newT()

        let defTree, defCons = genProofTreeAndCollectConstraint env def

        let subs1 = unify defCons
        let defTree = applyTree subs1 defTree // 在此处确定def的证明树 这是为了能实现Let多态
        let defType = typeOf defTree


        let genDefType = gen env defType // 泛型化defType的类型 即 MonoType -> PolyType

        let env, cons =
            match List.isEmpty othersId, defType with
            | true, _ -> addLetToEnv id genDefType env, []  // 给id绑定上泛型化的类型

            | false, Mono (T tvar) -> // 未确定变量的类型 只能全部绑上类型变量
                let idMono = newT()
                let othersIdMono = othersId |> List.map (fun _ -> newT())

                let env = addLetToEnv id (Mono idMono) env

                let cons = [(TTuple(idMono, othersIdMono), T tvar)]
                
                List.zip othersId othersIdMono
                |> List.fold (fun env (id', t) -> addLetToEnv id' (Mono t) env) env, cons

            // 如果othersId不为空 说明这个Let用于元组解构 那么要求def必须是一个元组
            // 这段代码就是把泛型化后的各个id的类型加到env里
            | false, Mono (TTuple (m1, othersM)) ->

                let genIdType = gen env (Mono m1)
                let genOthersIdType = othersM |> List.map (Mono >> gen env)

                let env = addLetToEnv id genIdType env
                
                List.zip othersId genOthersIdType
                |> List.fold (fun env (id', t) -> addLetToEnv id' t env) env, []

            | false, _ -> raise (failwith "let definien is not a tuple")

        let defTree = setType genDefType defTree

        let bodyTree, bodyCons = genProofTreeAndCollectConstraint env body
        let bodyType = monoTypeOf bodyTree
        
        let cons = (bodyType, letType) :: bodyCons @ cons @ defCons
        
        PLet (id, othersId, defTree, bodyTree, Mono letType), cons

    | LetRec (id, def, body) ->
        match def with
        | Lam _ ->
            let letType = newT() 

            let preDefType = TFun(newT(), newT()) // let rec 只支持定义函数

            let defEnv = addLetToEnv id (Mono preDefType) env // let rec 定义变量必须能被def的代码使用
            
            let defTree, defCons = genProofTreeAndCollectConstraint defEnv def

            let defCons = (preDefType, monoTypeOf defTree) :: defCons
            
            let subs1 = unify defCons
            let defTree = applyTree subs1 defTree // 在此处确定def的证明树 这是为了能实现Let多态
            let defType = typeOf defTree
            
            let genDefType = gen env defType // 泛型化defType的类型 如果存在自由变量 那么 MonoType -> PolyType
            let defTree = setType genDefType defTree
            let env = addLetToEnv id genDefType env
            
            let bodyTree, cons = genProofTreeAndCollectConstraint env body
            let bodyType = monoTypeOf bodyTree
            
            let cons = (bodyType, letType) :: cons @ defCons
            
            PLet (id, [], defTree, bodyTree, Mono letType), cons
        | _ -> raise (failwith "let rec support function only")

    | ADTDef (name, genList, constructors, expr) ->
        match tryFindType name env with
        | Some _ -> raise (failwithf "duplicate definition in type %s" name)
        | None ->
            let adtType, constructors = createADT name genList constructors env
            let env = addTypeToEnv name (adtType, constructors) env
            let env = Map.fold (fun env constructorName t -> addLetToEnv constructorName t env) env constructors
            let tree, cons = genProofTreeAndCollectConstraint env expr
            PADTDef(adtType, constructors, tree), cons

    | Case (case, branchs) ->

        let createPattern env (caseType: MonoType) (name, ids, e) =
            match (tryFindConstructor name env) |> Option.map inst, ids with
            | None, _ -> raise (failwithf "can not found the constructor '%s'" name)
            | Some (TFun _), [] -> raise (failwithf "pattern '%s' should have parameters" name)
            | Some adtType, [] ->
                (name, [], e), [(caseType, adtType)]
            | Some (TFun (t, adtType)), [id] ->
                let idType = newT()
                (name, [(id, Mono idType)], e), [(caseType, adtType); (t, idType)]
            | Some (TFun (t, adtType)), idList ->
                let idAndTypes = List.map (fun id -> id, Mono (newT())) idList
                let idsTuple = TTuple(List.head idAndTypes |> snd |> takeMono, List.tail idAndTypes |> List.map (snd >> takeMono))
                (name, idAndTypes, e), [(caseType, adtType); (t, idsTuple)]
            | Some _, _ ->
                raise (failwithf "pattern '%s' should not have parameters" name)

        let createBranchTree env (branchType: MonoType) (name, idAndTypes, e) =
            match idAndTypes with
            | [] ->
                let eTree, eCons = genProofTreeAndCollectConstraint env e
                let eType = monoTypeOf eTree
                (name, idAndTypes, eTree), (eType, branchType) :: eCons
            | [(id, idType)] -> 
                let env = addLetToEnv id idType env
                let eTree, eCons = genProofTreeAndCollectConstraint env e
                let eType = monoTypeOf eTree
                (name, idAndTypes, eTree), (eType, branchType) :: eCons
            | idAndTypes ->
                let env = List.fold (fun env (id, idType) -> addLetToEnv id idType env) env idAndTypes
                let eTree, eCons = genProofTreeAndCollectConstraint env e
                let eType = monoTypeOf eTree
                (name, idAndTypes, eTree), (eType, branchType) :: eCons

        let caseType = newT()

        let caseTree, caseCons = genProofTreeAndCollectConstraint env case

        let patternAndExpr, patternConsList = 
            branchs
            |> List.map (createPattern env caseType)
            |> List.unzip

        let cons = (monoTypeOf caseTree, caseType) :: (List.collect id (caseCons :: patternConsList))

        let subs = unify cons
        let caseTree = applyTree subs caseTree

        let patternAndExpr =
            List.map (fun (name, idAndTypes, e) -> name, List.map (fun (n, t) -> n, Mono (apply subs (takeMono t))) idAndTypes, e) patternAndExpr

        let branchType = newT()

        let brancheTreeList, branchConsList = 
            patternAndExpr
            |> List.map (createBranchTree env branchType)
            |> List.unzip

        let cons = List.collect id branchConsList @ cons
        

        PCase(caseTree, brancheTreeList, Mono branchType), cons

let infer (env: Enviorment) (expr: Expr) : ProofTree =
    let tree, cons = genProofTreeAndCollectConstraint env expr
    let subs = unify cons
    applyTree subs tree
