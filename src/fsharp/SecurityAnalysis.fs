module SecurityAnalysis
open Io.SecurityAnalysis
open Io.GCL
open AST

let rec Flow c  X = 
    match c with 
    | CSkip -> []
    | CAssign (a, e1) -> assign e1 a @ getX a X
    | CAAssign (a, e1,e2) -> assign e1 a @ assign e2 a @ getX a X
    | CSeq (a, b) ->  Flow a X @ Flow b X
    | CIf gc -> Flowgc gc X
    | CDo gc -> Flowgc gc X

and Flowgc gc X =
    match gc with
    | GCGuard (b, c) -> Flow c (X @ bexpr b)
    | GCChoice (gc1, gc2) -> Flowgc gc1 X @ Flowgc gc2 (X @ implicit gc1)

and implicit gc =
    match gc with
    | GCGuard (b, _) -> bexpr b
    | GCChoice (gc1, gc2) -> implicit gc1 @ implicit gc2

and bexpr b = 
    match b with
    | BTrue -> []
    | BFalse -> []
    | BAnd(b1, b2) -> bexpr b1 @ bexpr b2
    | BOr(b1, b2) -> bexpr b1 @ bexpr b2
    | BSAnd(b1, b2) -> bexpr b1 @ bexpr b2
    | BSOr(b1, b2) -> bexpr b1 @ bexpr b2
    | BNot b1 -> bexpr b1
    | BEq(a1, a2) -> aexp a1 @ aexp a2
    | BNeq(a1, a2) -> aexp a1 @ aexp a2
    | BGt(a1, a2) -> aexp a1 @ aexp a2
    | BGe(a1, a2) -> aexp a1 @ aexp a2
    | BLt(a1, a2) -> aexp a1 @ aexp a2
    | BLe(a1, a2) -> aexp a1 @ aexp a2

and aexp expr = 
    match expr with
    | AVar var -> [var]
    | APlus (expr1, expr2) -> aexp expr1 @ aexp expr2
    | AMinus (expr1, expr2) -> aexp expr1 @ aexp expr2
    | ATimes (expr1, expr2) -> aexp expr1 @ aexp expr2
    | ADiv (expr1, expr2) -> aexp expr1 @ aexp expr2
    | APow (expr1, expr2) -> aexp expr1 @ aexp expr2
    | AUMinus expr1 -> aexp expr1
    | AArray (string, expr1) -> aexp expr1
    | _ -> [] 

and assign expr too = 
    match expr with
    | AVar from -> [{from = from; into = too}]
    | APlus (expr1, expr2) -> assign expr1 too @ assign expr2 too
    | AMinus (expr1, expr2) -> assign expr1 too @ assign expr2 too
    | ATimes (expr1, expr2) -> assign expr1 too @ assign expr2 too
    | ADiv (expr1, expr2) -> assign expr1 too @ assign expr2 too
    | APow (expr1, expr2) -> assign expr1 too @ assign expr2 too
    | AUMinus expr1 -> assign expr1 too
    | AArray (string, expr1) -> assign expr1 too
    | _ -> [] 

and getX too X = 
    match X with
    | e1 :: e2 -> {from = e1; into = too} :: getX too e2
    | [] -> []


let transitiveClosure (rules: List<Flow>) : Set<string * string> =
    let levels =
        rules |> List.fold (fun acc r -> acc |> Set.add r.from |> Set.add r.into) Set.empty
    let mutable result =
        rules |> List.map (fun r -> (r.from, r.into)) |> Set.ofList
    result <- Set.fold (fun acc l -> Set.add (l, l) acc) result levels
    let levList = Set.toList levels
    for k in levList do
        for i in levList do
            for j in levList do
                if Set.contains (i, k) result && Set.contains (k, j) result then
                    result <- Set.add (i, j) result
    result

let allowedVarFlows (classification: Map<string, string>) (levelClosure: Set<string * string>) : List<Flow> =
    [ for x in classification.Keys do
        for y in classification.Keys do
            if Set.contains (classification.[x], classification.[y]) levelClosure then
                yield { from = x; into = y } ]

let getFlow input =
    match Parser.parse Grammar.start_commands input.commands with
        | Ok ast ->
            Flow ast []
        | Error e -> []


let analysis (input: Input) : Output =
    let actual =  getFlow input

    let levelClosure = transitiveClosure input.lattice.rules
    let allowed = allowedVarFlows input.classification levelClosure

    let allowedSet = allowed |> List.map (fun f -> (f.from, f.into)) |> Set.ofList
    let violations = actual |> List.filter (fun f -> not (Set.contains (f.from, f.into) allowedSet))
    { actual = actual
      allowed = allowed
      violations = violations
      is_secure = List.isEmpty violations }
