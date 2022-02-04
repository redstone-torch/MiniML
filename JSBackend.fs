module JSBackend

open Type
open ProofTree

let generateLit (lit: Lit) =
    match lit with
    | LUnit -> "undefined"
    | LInt i -> string i
    | LBool b -> if b then "true" else "false"
    | LString s -> "'" + s + "'"
    | LReal r -> string r

let (|Operator|_|) tree =
    match tree with
    | PApp(PApp(PVal (op, _), left, _), right, _) ->
        match op with
        | "+." | "-." | "*." | "/." | "<." | "<=." | ">." | ">=."  -> Some (left, op.Substring(0, op.Length - 1), right)
        | "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "::" | "++" | "@"
            -> Some (left, op, right)
        | _ -> None
    | _ -> None

let rec generateTree (tree: ProofTree) =
    match tree with
    | PLit (l, _) -> generateLit l
    | PVal (id, _) -> id
    | PTuple (tr1, othersTr, _) ->
        "["
        + generateTree tr1
        + List.fold (fun s tr -> s + "," + generateTree tr) "" othersTr
        + "]"
    | PList (list, _) ->
        match list with
        | [] -> "$emptyList"
        | e :: xs ->
            "$createList(["
            + generateTree e
            + List.fold (fun s tr -> s + "," + generateTree tr) "" xs
            + "])"

    | Operator (left, op, right) ->
        match monoTypeOf left, op with
        | TPrim PTInt, "/" -> "~~(" + generateTree left + "/" + generateTree right + ")"
        | _ ->
            match op with
            | "::" -> "$Cons(" + generateTree left + "," + generateTree right + ")"
            | "++" -> "$concat(" + generateTree left + "," + generateTree right + ")"
            | "@"  -> "(" + generateTree left + "+" + generateTree right + ")"
            | op -> generateOperator left op right

    | PApp (func, arg, _) ->
        generateTree func + "(" + generateTree arg + ")"

    | PLam (param, body, _) ->
        "(" + param + "=>" + generateTree body + ")"

    | PIfElse (cond, ifBranch, elseBranch, _) ->
        "(" + generateTree cond + ")?(" + generateTree ifBranch + "):(" + generateTree elseBranch + ")"

    | PLet (id, othersId, def, body, _) ->
        match othersId with
        | [] ->
            "(()=>{"
            + "let " + id + "=" + generateTree def + ";"
            + "return " + generateTree body
            + ";})()"
        | othersId ->
            "(()=>{"
            + "let $t=" + generateTree def + ";"
            + fst (List.fold (fun (s, i) id -> (s + "let " + id + "=$t[" + string i + "];", i + 1))  ("", 0) (id :: othersId))
            + "return " + generateTree body + ";"
            + "})()"

    | PCaseList (list, e, xs, someBranch, emptyBranch, _) ->
        "(() => {"
        + "let $list = " + generateTree list + ";"
        + "let " + e + "=$list.value;"
        + "let " + xs + "=$list.tail;"
        + "return ($list.isTail) ? (" + generateTree emptyBranch + ") : (" + generateTree someBranch + ") ;"
        + "})()"

    | PADTDef (_, constructors, tree) ->
        "(() => {"
        + Map.fold (fun s c t -> 
                match t with
                | Poly (_, TFun _) | Mono (TFun _) -> s + "let " + c + "=$adtConstructor('" + c + "');"
                | _ -> "let " + c + "=$adt('" + c + "');"
            ) "" constructors
        + "return " + generateTree tree + ";"
        + "})()"

    | PCase (case, branches, _) ->
        let generateBranch (name, idList, tree) =
            " if ($case.cname==='" + name + "') return "
            +   match idList with
                | [] -> generateTree tree + ""
                | [(id, _)] -> "(" + id + "=>" + generateTree tree + ")($case.value)"
                | idList ->
                    "(()=>{"
                    + fst (List.fold (fun (s, i) (id, _) -> (s + "let " + id + "=$case.value[" + string i + "];", i + 1))  ("", 0) idList)
                    + "return " + generateTree tree + ";"
                    + "})()"
            + ";else "

        "(() => {"
        + "let $case = " + generateTree case + ";"
        + List.fold (fun s tuple -> s + generateBranch tuple) "" branches
        + "throw new Error('unmatch pattern in case of');"
        + "})()"
        
    | _ -> raise (failwith "unspported code")

and generateOperator left op right =
    "("
    + generateTree left
    + op
    + generateTree right
    + ")"

let stdLibUnzip = """

let $emptyList = { isTail: true };
let $createList = array => {
    var list = $emptyList;
	for (var i = array.length - 1; i >= 0; i--) {
		list = { value: array[i], tail: list }
	}
    return list;
};

let $reverse = list => {
	let res = $emptyList;
	let e = list;
    while (e !== $emptyList) {
		res = $Cons(e.value, res);
		e = e.tail;
	}
	return res;
};

let $Cons = (v, list) => { return { value: v, tail: list } };
let $concat = (list1, list2) => {
	list1 = $reverse(list1);
	while (!list1.isTail)
	{
		list2 = $Cons(list1.value, list2);
		list1 = list1.tail;
	}
	return list2;
};

let $adt = (constructorName) => { return { cname: constructorName } };
let $adtConstructor = constructorName => value => { return { cname: constructorName, value: value } };

let string = (obj) => {
    if (obj instanceof Array) {
        let res = '(' + string(obj[0]);
        for (var i = 1; i < obj.length; i++) {
            res += "," + string(obj[i]);
        }
        return res + ')';
    } else if (typeof obj === 'object') {
        if (obj === $emptyList) {
            return '[]';
        } else if (obj.tail) {
            let res = '[' + string(obj.value);
            obj = obj.tail;
            while (obj !== $emptyList) {
                res += ";" + string(obj.value);
                obj = obj.tail;
            }
            return res + ']';
        } else if (obj.cname !== undefined) {
            return obj.cname + ':' + string(obj.value);
        }
    }
    return String(obj);
};

let int = (r) => ~~r;
let real = (i) => i;

let neg = n => -n;

let intOptionOfString = (str) => {
    let res = parseInt(str); return Number.isNaN(res) ? None : Some(res)
};

let intOfString = (str) => {
    let res = parseInt(str);
    if (Number.isNaN(res)) throw new Error('cannot parse `' + str + '` to int');
    return res
};

let None = $adt('None');
let Some = $adtConstructor('Some');

let reverse = $reverse;

let filter = (f = > (list = > (() = > {
	let loop = (f = > (res = > (list = > (() = > {
		let $list = list;
		let e = $list.value;
		let xs = $list.tail;
		return ($list.isTail) ? (res) : ((f(e)) ? (loop(f)($Cons(e, res))(xs)) : (loop(f)(res)(xs)));
	})())));
	return reverse(loop(f)($emptyList)(list));
})()));

let initList = (size) => (f) => {
	let arr = [];
	for (var i = 0; i < size; i++) {
		arr.push(f(i));
	}
	return $createList(arr);
};

"""

let stdLib = """
let $emptyList={isTail:true};let $createList=array=>{var list=$emptyList;for(var i=array.length-1;i>=0;i--){list={value:array[i],tail:list}}return list};let $reverse=list=>{let res=$emptyList;let e=list;while(e!==$emptyList){res=$Cons(e.value,res);e=e.tail}return res};let $Cons=(v,list)=>{return{value:v,tail:list}};let $concat=(list1,list2)=>{list1=$reverse(list1);while(!list1.isTail){list2=$Cons(list1.value,list2);list1=list1.tail}return list2};let $adt=(constructorName)=>{return{cname:constructorName}};let $adtConstructor=constructorName=>value=>{return{cname:constructorName,value:value}};let string=(obj)=>{if(obj instanceof Array){let res='('+string(obj[0]);for(var i=1;i<obj.length;i++){res+=","+string(obj[i])}return res+')'}else if(typeof obj==='object'){if(obj===$emptyList){return'[]'}else if(obj.tail){let res='['+string(obj.value);obj=obj.tail;while(obj!==$emptyList){res+=";"+string(obj.value);obj=obj.tail}return res+']'}else if(obj.cname!==undefined){return obj.cname+':'+string(obj.value)}}return String(obj)};let int=(r)=>~~r;let real=(i)=>i;let neg=n=>-n;let intOptionOfString=(str)=>{let res=parseInt(str);return Number.isNaN(res)?None:Some(res)};let intOfString=(str)=>{let res=parseInt(str);if(Number.isNaN(res))throw new Error('cannot parse `'+str+'` to int');return res};let None=$adt('None');let Some=$adtConstructor('Some');let reverse=$reverse;let filter=(f=>(list=>(()=>{let loop=(f=>(res=>(list=>(()=>{let $list=list;let e=$list.value;let xs=$list.tail;return($list.isTail)?(res):((f(e))?(loop(f)($Cons(e,res))(xs)):(loop(f)(res)(xs)))})())));return reverse(loop(f)($emptyList)(list))})()));let initList=(size)=>(f)=>{let arr=[];for(var i=0;i<size;i++){arr.push(f(i))}return $createList(arr)};
let print = console.log;
let readline = prompt;

"""

let generate (tree: ProofTree) =
    stdLib
    + generateTree tree