module Interpreter
open Io.Interpreter
open Io.GCL
open AST
open Calculator

let min32 = System.Int32.MinValue
let max32 = System.Int32.MaxValue
let in_range (v: bigint) = v >= bigint min32 && v <= bigint max32
let to_int32_opt (v: bigint) = if in_range v then Some (int32 v) else None

// Evaluate arithmetic expressions
let rec eval_aexpr (a: aexpr) (vars: Map<string, int32>) (arrays: Map<string, List<int32>>) : int32 option =
    let (>>=) x f = Option.bind f x
    match a with
    | ANum n -> to_int32_opt (int64 n)
    | AVar x ->
        match Map.tryFind x vars with
        | Some v -> Some v
        | None -> Some 0
    | AArray (name, idx) ->
        eval_aexpr idx vars arrays >>= fun i ->
        match Map.tryFind name arrays with
        | Some arr when i >= 0 && i < List.length arr -> Some arr.[i]
        | _ -> Some 0
    | APlus (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 ->
        match Calculator.evaluate (PlusExpr(Num v1, Num v2)) with
        | Ok result when result >= min32 && result <= max32 -> Some result
        | _ -> None
    | AMinus (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 ->
        match Calculator.evaluate (MinusExpr(Num v1, Num v2)) with
        | Ok result when result >= min32 && result <= max32 -> Some result
        | _ -> None
    | ATimes (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 ->
        match Calculator.evaluate (TimesExpr(Num v1, Num v2)) with
        | Ok result when result >= min32 && result <= max32 -> Some result
        | _ -> None
    | ADiv (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 ->
        match Calculator.evaluate (DivExpr(Num v1, Num v2)) with
        | Ok result when result >= min32 && result <= max32 -> Some result
        | _ -> None
    | APow (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 ->
        match Calculator.evaluate (PowExpr(Num v1, Num v2)) with
        | Ok result when result >= min32 && result <= max32 -> Some result
        | _ -> None
    | AUMinus a1 ->
        eval_aexpr a1 vars arrays >>= fun v ->
        match Calculator.evaluate (UMinusExpr(Num v)) with
        | Ok result when result >= min32 && result <= max32 -> Some result
        | _ -> None

// Evaluate boolean expression
let rec eval_bexpr (b: bexpr) (vars: Map<string, int32>) (arrays: Map<string, List<int32>>) : bool option =
    let (>>=) x f = Option.bind f x
    match b with
    | BTrue -> Some true
    | BFalse -> Some false
    | BAnd (b1, b2) ->
        eval_bexpr b1 vars arrays >>= fun v1 ->
        if not v1 then Some false else eval_bexpr b2 vars arrays
    | BOr (b1, b2) ->
        eval_bexpr b1 vars arrays >>= fun v1 ->
        if v1 then Some true else eval_bexpr b2 vars arrays
    | BSAnd (b1, b2) ->
        eval_bexpr b1 vars arrays >>= fun v1 ->
        if not v1 then Some false else eval_bexpr b2 vars arrays
    | BSOr (b1, b2) ->
        eval_bexpr b1 vars arrays >>= fun v1 ->
        if v1 then Some true else eval_bexpr b2 vars arrays
    | BNot b1 -> eval_bexpr b1 vars arrays >>= fun v -> Some (not v)
    | BEq (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 -> Some (v1 = v2)
    | BNeq (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 -> Some (v1 <> v2)
    | BGt (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 -> Some (v1 > v2)
    | BGe (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 -> Some (v1 >= v2)
    | BLt (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 -> Some (v1 < v2)
    | BLe (a1, a2) ->
        eval_aexpr a1 vars arrays >>= fun v1 ->
        eval_aexpr a2 vars arrays >>= fun v2 -> Some (v1 <= v2)

// Simulate command and update memory
let apply_command label (memory: InterpreterMemory) : InterpreterMemory option =
    match label with
    | Compiler.CommandLabel c ->
        match c with
        | CAssign (x, a) ->
            eval_aexpr a memory.variables memory.arrays |> Option.bind (fun v ->
                if v < min32 || v > max32 then None
                else Some { memory with variables = Map.add x v memory.variables })
        | CAAssign (arr, idx, a) ->
            eval_aexpr idx memory.variables memory.arrays |> Option.bind (fun i ->
            eval_aexpr a memory.variables memory.arrays |> Option.bind (fun v ->
                if v < min32 || v > max32 then None
                else
                    match Map.tryFind arr memory.arrays with
                    | Some arrList when i >= 0 && i < List.length arrList ->
                        let updated = arrList |> List.mapi (fun j x -> if j = i then v else x)
                        Some { memory with arrays = Map.add arr updated memory.arrays }
                    | _ -> None))
        | _ -> Some memory
    | _ -> Some memory

// Get list of edges
let get_edgeList startnote endnote input =
      match Parser.parse Grammar.start_commands input.commands with
        | Ok ast ->
            Compiler.edges ast startnote endnote input.determinism
        | Error e -> []

// Get map of nodes and their edges
let groupEdgesBySource (edgeList: Compiler.edge list) : Map<string, Compiler.edge list> =
    edgeList
    |> List.fold (fun acc edge ->
        let src = edge.source
        match Map.tryFind src acc with
        | Some edges -> Map.add src (edge :: edges) acc
        | None -> Map.add src [edge] acc
    ) Map.empty

let analysis (input: Input) : Output =
    let ini_note = "start"
    let end_note = "end"
    let trace_length = input.trace_length
    let edgeList = get_edgeList ini_note end_note input
    let sourceEdgeMap : Map<string, Compiler.edge list> = groupEdgesBySource edgeList

    let outgoing_edges node =
        match Map.tryFind node sourceEdgeMap with
        | Some edges -> edges
        | None -> []

    let is_guard_enabled label (memory: InterpreterMemory) =
        match label with
        | Compiler.BexprLabel b ->
            // Debug output: print the guard and memory
            printfn "Evaluating guard: %s" (Compiler.printL label)
            printfn "Memory: %A" memory.variables
            match eval_bexpr b memory.variables memory.arrays with
            | Some result ->
                printfn "Result: %b" result
                Some result
            | None ->
                printfn "Result: <stuck>"
                None
        | _ -> Some true

    let rec exec_steps node memory trace steps_left =
        if node = end_note then
            (List.rev trace, TerminationState.Terminated, node)
        elif steps_left = 0 then
            (List.rev trace, TerminationState.Running, node)
        else
            let edges = outgoing_edges node
            // Only keep edges where guard is Some true
            let enabled = edges |> List.choose (fun e ->
                match is_guard_enabled e.label memory with
                | Some true -> Some e
                | _ -> None)
            match enabled with
            | [] -> (List.rev trace, TerminationState.Stuck, node)
            | e::_ ->
                match apply_command e.label memory with
                | Some new_memory ->
                    let step = { action = Compiler.printL e.label; node = e.target; memory = new_memory }
                    exec_steps e.target new_memory (step :: trace) (steps_left - 1)
                | None -> (List.rev trace, TerminationState.Stuck, node)

    let initial_memory = input.assignment
    let trace, termination, final_node = exec_steps ini_note initial_memory [] trace_length
    let compilerInput : Io.Compiler.Input = { commands = input.commands; determinism = input.determinism }
    let pg = Compiler.analysis compilerInput

    { initial_node = ini_note;
      final_node = final_node;
      dot = pg.dot;
      trace = trace;
      termination = termination }
