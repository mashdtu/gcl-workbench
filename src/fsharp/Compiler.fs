module Compiler
open Io.Compiler
open Io.GCL
open FSharp.Text.Lexing
open System
open AST
open Parser

type Label = 
    CommandLabel of command | BexprLabel of bexpr

type edge = {
    source : string
    label  : Label
    target : string
}
let counter = ref 0
let intcounter = ref 0

let nextint() =
    intcounter.Value <- intcounter.Value + 1
    "intq" + string intcounter.Value

let nextedge() =
    counter.Value <- counter.Value + 1
    "q" + string counter.Value

let resetCounters () =
    counter.Value <- 0
    intcounter.Value <- 0

let rec edges c q1 q2  det =
    match c with
    | CSkip -> [{source = q1; label = CommandLabel CSkip; target = q2}]
    | CAssign  a  -> [{source = q1; label = CommandLabel (CAssign a); target = q2}]
    | CAAssign  a  -> [{source = q1; label = CommandLabel (CAAssign a); target = q2}]
    | CSeq (a,b) -> 
            let intq = nextint()
            let aedges = edges a q1 intq det
            let qmid = nextedge()
            let a = aupdate aedges qmid intq
            a @ edges b qmid q2 det
    | CIf gc -> gcedges gc q1 q2 det
    | CDo gc -> gcedges gc q1 q1 det @ [{source = q1; label = BexprLabel (antiBexpr gc); target = q2}]

and gcedges gc q1 q2  det = 
    match det with
    | NonDeterministic ->   
        match gc with
        | GCGuard (b, c) ->
            let qnum = nextedge()
            [{source = q1; label = BexprLabel b; target = qnum}] @ edges c qnum q2 det
        | GCChoice (gc1, gc2) ->
                gcedges gc1 q1 q2 det @ gcedges gc2 q1 q2 det

    | Deterministic ->
        match gc with
        | GCGuard (b, c) ->
            let qnum = nextedge()
            [{source = q1; label = BexprLabel  (BAnd (b, BNot BFalse)); target = qnum}] @ edges c qnum q2 det
        | GCChoice (gc1, gc2) ->
            detgcedges gc1 q1 q2  BFalse @ detgcedges gc2 q1 q2 (BOr (getBexpr gc1, BFalse))

and detgcedges gc q1 q2 beta=  
    match gc with
        | GCGuard (b, c) ->
            let qnum = nextedge()
            [{source = q1; label = BexprLabel (BAnd (b, BNot beta)); target = qnum}] @ edges c qnum q2 Deterministic
        | GCChoice (gc1, gc2) ->
            detgcedges gc1 q1 q2 beta @ detgcedges gc2 q1 q2 (BOr (getBexpr gc1, beta))

and aupdate list qmid intq =
    match List.rev list with
    | [] -> []
    | {source = s; label = l; target = t} :: rest ->  
        if t = intq then {source = s; label = l; target = qmid} :: aupdate rest qmid intq
        else {source = s; label = l; target = t}  :: aupdate rest qmid intq

and antiBexpr gc =
    match gc with
    | GCGuard (b, _) -> BNot b
    | GCChoice (gc1, gc2) -> BAnd (antiBexpr gc1, antiBexpr gc2)

and getBexpr gc=
    match gc with
    | GCGuard (b, _) -> b
    | GCChoice (gc1, gc2) -> BOr (getBexpr gc1, getBexpr gc2)

let printL label =
    match label with
    | CommandLabel CSkip -> "skip"
    | CommandLabel (CAssign (string,aexpress)) ->  string + ":= " + prettyPrintA aexpress
    | CommandLabel (CAAssign (string,aexpress1,aexpress2)) ->  string + "[" + prettyPrintA aexpress1 + "]" + ":= " + prettyPrintA aexpress2
    | BexprLabel (b) -> prettyPrintB b
    | _ -> "TODO"
let rec printDotedges e =
    match e with
    | e1::e2 -> e1.source + " -> " + e1.target + "[label = \"" + printL e1.label+ "\"];\n" + printDotedges e2
    | _ -> ""

let printDot e =
    match e with
    | _ -> "digraph program_graph {rankdir=LR;\nnode [shape = circle]\n"
            + printDotedges e
            + "}"
         

let analysis (input: Input) : Output =
    resetCounters ()
    match parse Grammar.start_commands input.commands with
        | Ok ast ->
            {dot = printDot(edges ast "start" "end" input.determinism)}
        | Error e -> { dot = "" }