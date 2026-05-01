module RiscV
open Io.RiscV
open Io.GCL
open AST
open Parser
open Compiler

// Collect all variable names referenced in edges
let rec private varsInA a =
    match a with
    | AVar x -> Set.singleton x
    | ANum _ -> Set.empty
    | APlus(a1,a2) | AMinus(a1,a2) | ATimes(a1,a2) | ADiv(a1,a2) | APow(a1,a2) ->
        Set.union (varsInA a1) (varsInA a2)
    | AUMinus a -> varsInA a
    | _ -> Set.empty

let rec private varsInB b =
    match b with
    | BEq(a1,a2)|BNeq(a1,a2)|BGt(a1,a2)|BGe(a1,a2)|BLt(a1,a2)|BLe(a1,a2) ->
        Set.union (varsInA a1) (varsInA a2)
    | BNot b -> varsInB b
    | BAnd(b1,b2)|BOr(b1,b2)|BSAnd(b1,b2)|BSOr(b1,b2) ->
        Set.union (varsInB b1) (varsInB b2)
    | _ -> Set.empty

let rec private varsInC c =
    match c with
    | CAssign(x,a) -> Set.add x (varsInA a)
    | CSeq(c1,c2) -> Set.union (varsInC c1) (varsInC c2)
    | CIf gc | CDo gc -> varsInGC gc
    | _ -> Set.empty

and private varsInGC gc =
    match gc with
    | GCGuard(b,c) -> Set.union (varsInB b) (varsInC c)
    | GCChoice(gc1,gc2) -> Set.union (varsInGC gc1) (varsInGC gc2)

let private collectVars (edgeList: edge list) =
    edgeList |> List.fold (fun acc (e: edge) ->
        match e.label with
        | CommandLabel c -> Set.union acc (varsInC c)
        | BexprLabel b   -> Set.union acc (varsInB b)
    ) Set.empty

// Load aexpr into reg
let private loadA (a: aexpr) (reg: string) : string list =
    match a with
    | ANum n -> [sprintf "\tli %s, %d" reg (int n)]
    | AVar x -> [sprintf "\tlw %s, v%s" reg x]
    | _ -> [sprintf "\t# cannot load into %s" reg]

// Emit x := base ^ exp
let private emitPow (x: string) (base_: aexpr) (exp: aexpr) : string list =
    [sprintf "\tla t0, v%s" x]
    @ loadA base_ "t1"
    @ loadA exp   "t2"
    @ ["\tli a7, 10"
       "\tecall"
       "\tsw t1, 0(t0)"]

// Emit RISC-V instructions for an assignment command (no jump)
// Convention: t0 = address of dest, t1 = value, t2 = scratch for binary ops
let private emitCmd (c: command) : string list =
    match c with
    | CSkip -> []
    | CAssign(x, ANum n) ->
        [sprintf "\tla t0, v%s" x
         sprintf "\tli t1, %d" (int n)
         "\tsw t1, 0(t0)"]
    | CAssign(x, AVar y) ->
        [sprintf "\tla t0, v%s" x
         sprintf "\tlw t1, v%s" y
         "\tsw t1, 0(t0)"]
    | CAssign(x, AUMinus(ANum n)) ->
        [sprintf "\tla t0, v%s" x
         sprintf "\tli t1, %d" (-(int n))
         "\tsw t1, 0(t0)"]
    | CAssign(x, AUMinus(AVar y)) ->
        [sprintf "\tla t0, v%s" x
         sprintf "\tla t1, v%s" y
         "\tlw t1, 0(t1)"
         "\tneg t1, t1"
         "\tsw t1, 0(t0)"]
    | CAssign(x, APlus(a1,a2)) ->
        [sprintf "\tla t0, v%s" x] @ loadA a1 "t1" @ loadA a2 "t2" @
        ["\tadd t1, t1, t2"; "\tsw t1, 0(t0)"]
    | CAssign(x, AMinus(a1,a2)) ->
        [sprintf "\tla t0, v%s" x] @ loadA a1 "t1" @ loadA a2 "t2" @
        ["\tsub t1, t1, t2"; "\tsw t1, 0(t0)"]
    | CAssign(x, ATimes(a1,a2)) ->
        [sprintf "\tla t0, v%s" x] @ loadA a1 "t1" @ loadA a2 "t2" @
        ["\tmul t1, t1, t2"; "\tsw t1, 0(t0)"]
    | CAssign(x, ADiv(a1,a2)) ->
        [sprintf "\tla t0, v%s" x] @ loadA a1 "t1" @ loadA a2 "t2" @
        ["\tdiv t1, t1, t2"; "\tsw t1, 0(t0)"]
    | CAssign(x, APow(a1,a2)) ->
        emitPow x a1 a2
    | _ -> ["\t# unhandled command"]

let private emitBranch (b: bexpr) (target: string) : string list =
    // load a1→t0, a2→t1  (used for eq/ne/lt where order matches the mnemonic)
    let ld a1 a2 = loadA a1 "t0" @ loadA a2 "t1"
    // load a1→t1, a2→t0  (swap so "blt t0,t1" reads as a2<a1 ↔ a1>a2)
    let ldSwap a1 a2 = loadA a1 "t1" @ loadA a2 "t0"
    match b with
    | BTrue              -> [sprintf "\tj %s" target]
    | BFalse             -> []
    | BEq(a1,a2)         -> ld a1 a2     @ [sprintf "\tbeq t0, t1, %s" target]
    | BNeq(a1,a2)        -> ld a1 a2     @ [sprintf "\tbne t0, t1, %s" target]
    // a1 > a2  ↔  a2 < a1: swap loads, then blt t0,t1
    | BGt(a1,a2)         -> ldSwap a1 a2 @ [sprintf "\tblt t0, t1, %s" target]
    // a1 >= a2: equal OR a1>a2
    | BGe(a1,a2)         -> ld a1 a2     @ [sprintf "\tbeq t0, t1, %s" target;
                                             sprintf "\tblt t1, t0, %s" target]
    | BLt(a1,a2)         -> ld a1 a2     @ [sprintf "\tblt t0, t1, %s" target]
    // a1 <= a2: equal OR a1<a2
    | BLe(a1,a2)         -> ld a1 a2     @ [sprintf "\tbeq t0, t1, %s" target;
                                             sprintf "\tblt t0, t1, %s" target]
    | BNot BTrue         -> []
    | BNot BFalse        -> [sprintf "\tj %s" target]
    | BNot(BEq(a1,a2))   -> ld a1 a2     @ [sprintf "\tbne t0, t1, %s" target]
    | BNot(BNeq(a1,a2))  -> ld a1 a2     @ [sprintf "\tbeq t0, t1, %s" target]
    // NOT(a1>a2) = a1<=a2: same as BLe
    | BNot(BGt(a1,a2))   -> ld a1 a2     @ [sprintf "\tbeq t0, t1, %s" target;
                                             sprintf "\tblt t0, t1, %s" target]
    // NOT(a1<a2) = a1>=a2: same as BGe
    | BNot(BLt(a1,a2))   -> ld a1 a2     @ [sprintf "\tbeq t0, t1, %s" target;
                                             sprintf "\tblt t1, t0, %s" target]
    // NOT(a1>=a2) = a1<a2: same as BLt
    | BNot(BGe(a1,a2))   -> ld a1 a2     @ [sprintf "\tblt t0, t1, %s" target]
    // NOT(a1<=a2) = a1>a2: same as BGt
    | BNot(BLe(a1,a2))   -> ldSwap a1 a2 @ [sprintf "\tblt t0, t1, %s" target]
    | _                  -> ["\t# unsupported bool"]

// Map node names to RISC-V labels
let private lbl n =
    match n with
    | "start" -> "qStart"
    | "end"   -> "qFinal"
    | _       -> n

// Emit all RISC-V lines for a single program-graph node
let private emitNode (n: string) (out: edge list) : string list =
    if n = "end" then
        ["qFinal:"; "\tli a7, 10"; "\tecall"]
    else
        let header = [sprintf "%s:" (lbl n)]
        match out with
        | [] ->
            header @ ["\tli a7, 10"; "\tecall"]
        | [e] ->
            match e.label with
            | CommandLabel c -> header @ emitCmd c @ [sprintf "\tj %s" (lbl e.target)]
            | BexprLabel b   -> header @ emitBranch b (lbl e.target)
        | [e1; e2] ->
            match e1.label, e2.label with
            | BexprLabel b1, BexprLabel _ ->
                header
                @ emitBranch b1 (lbl e1.target)
                @ [sprintf "\tj %s" (lbl e2.target)]
            | _ -> header @ ["\t# unexpected edge combination"]
        | _ -> header @ ["\t# too many outgoing edges"]

// BFS to get a stable node order starting from "start"
let private bfsOrder (start: string) (edgeMap: Map<string, edge list>) =
    let rec go q visited acc =
        match q with
        | [] -> List.rev acc
        | n :: rest when Set.contains n visited -> go rest visited acc
        | n :: rest ->
            let succs =
                match Map.tryFind n edgeMap with
                | Some es -> List.map (fun (e: edge) -> e.target) es
                | None -> []
            go (rest @ succs) (Set.add n visited) (n :: acc)
    go [start] Set.empty []

let private getRiscV (ast: command) : string =
    resetCounters ()
    let edgeList = edges ast "start" "end" NonDeterministic
    let edgeMap =
        List.fold (fun acc (e: edge) ->
            let cur = match Map.tryFind e.source acc with Some es -> es | None -> []
            Map.add e.source (cur @ [e]) acc
        ) Map.empty edgeList
    let nodes    = bfsOrder "start" edgeMap
    let allNodes = if List.contains "end" nodes then nodes else nodes @ ["end"]
    let textLines =
        [".text"]
        @ List.collect (fun n ->
            emitNode n (match Map.tryFind n edgeMap with Some es -> es | None -> [])
        ) allNodes
    // Collect variable names in first-appearance order by scanning emitted instructions
    let varsInOrder =
        textLines
        |> List.choose (fun line ->
            let idx = line.IndexOf(", v")
            if idx < 0 then None
            else
                let rest = line.[idx + 3 ..]
                let name = rest |> Seq.takeWhile (fun c -> c = '_' || System.Char.IsLetterOrDigit c) |> System.String.Concat
                if name.Length > 0 then Some name else None)
        |> List.fold (fun (seen, acc) v ->
            if Set.contains v seen then (seen, acc)
            else (Set.add v seen, acc @ [v])
        ) (Set.empty, [])
        |> snd
    let dataLines =
        ".data" :: (varsInOrder |> List.map (sprintf "v%s:\t\t.word 0"))
    String.concat "\n" (dataLines @ textLines)

let analysis (input: Input) : Output =
    match parse Grammar.start_commands input.commands with
    | Error _ -> { assembly = "" }
    | Ok ast  -> { assembly = getRiscV (BiGCL.getbinaryRV ast) }
