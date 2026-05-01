module Parser
open Io.Parser

open FSharp.Text.Lexing
open System
open AST

exception ParseError of Position * string * Exception

let parse parser src =
    let lexbuf = LexBuffer<char>.FromString src

    let parser = parser Lexer.tokenize

    try
        Ok(parser lexbuf)
    with
    | e ->
        let pos = lexbuf.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = e.Message
        let lastToken = new String(lexbuf.Lexeme)
        eprintf "Parse failed at line %d, column %d:\n" line column
        eprintf "Last token: %s" lastToken
        eprintf "\n"
        Error(ParseError(pos, lastToken, e))

let rec prettyPrintA (a: aexpr) : string =
    match a with
    | ANum n -> string n
    | AVar x -> x
    | AArray(name, idx) -> sprintf "%s[%s]" name (prettyPrintA idx)
    | APlus(a1, a2) -> sprintf "(%s + %s)" (prettyPrintA a1) (prettyPrintA a2)
    | AMinus(a1, a2) -> sprintf "(%s - %s)" (prettyPrintA a1) (prettyPrintA a2)
    | ATimes(a1, a2) -> sprintf "(%s * %s)" (prettyPrintA a1) (prettyPrintA a2)
    | ADiv(a1, a2) -> sprintf "(%s / %s)" (prettyPrintA a1) (prettyPrintA a2)
    | APow(a1, a2) -> sprintf "(%s ^ %s)" (prettyPrintA a1) (prettyPrintA a2)
    | AUMinus a1 ->
        match a1 with
        | ANum _ | AVar _ | AArray _ -> sprintf "-%s" (prettyPrintA a1)
        | APlus(ANum n, rhs) -> sprintf "(-%s + %s)" (string n) (prettyPrintA rhs)
        | APow(ANum n, rhs) -> sprintf "(-%s ^ %s)" (string n) (prettyPrintA rhs)
        // Normalize parser shapes like -(99 / x) and -(25 - d) into -99 / x and -25 - d
        | ADiv(ANum n, rhs) -> sprintf "(-%s / %s)" (string n) (prettyPrintA rhs)
        | AMinus(ANum n, rhs) -> sprintf "(-%s - %s)" (string n) (prettyPrintA rhs)
        | _ -> sprintf "-%s" (prettyPrintA a1)

let rec prettyPrintB (b: bexpr) : string =
    match b with
    | BTrue -> "true"
    | BFalse -> "false"
    | BAnd(b1, b2) -> sprintf "(%s & %s)" (prettyPrintB b1) (prettyPrintB b2)
    | BOr(b1, b2) -> sprintf "(%s | %s)" (prettyPrintB b1) (prettyPrintB b2)
    | BSAnd(b1, b2) -> sprintf "(%s && %s)" (prettyPrintB b1) (prettyPrintB b2)
    | BSOr(b1, b2) -> sprintf "(%s || %s)" (prettyPrintB b1) (prettyPrintB b2)
    | BNot b1 -> sprintf "!%s" (prettyPrintB b1)
    | BEq(a1, a2) -> sprintf "(%s = %s)" (prettyPrintA a1) (prettyPrintA a2)
    | BNeq(a1, a2) -> sprintf "(%s != %s)" (prettyPrintA a1) (prettyPrintA a2)
    | BGt(a1, a2) -> sprintf "(%s > %s)" (prettyPrintA a1) (prettyPrintA a2)
    | BGe(a1, a2) -> sprintf "(%s >= %s)" (prettyPrintA a1) (prettyPrintA a2)
    | BLt(a1, a2) -> sprintf "(%s < %s)" (prettyPrintA a1) (prettyPrintA a2)
    | BLe(a1, a2) -> sprintf "(%s <= %s)" (prettyPrintA a1) (prettyPrintA a2)

// Precedence levels: 1=||/|, 2=&&/&, 3=!/atoms
and bprec (b: bexpr) : int =
    match b with
    | BSOr _ | BOr _ -> 1
    | BSAnd _ | BAnd _ -> 2
    | _ -> 3

// Left operand: parens if strictly lower precedence
and prettyPrintBLeftPrec (prec: int) (b: bexpr) : string =
    if bprec b < prec then sprintf "(%s)" (prettyPrintB b)
    else prettyPrintB b

// Right operand: parens if lower or equal precedence (left-assoc)
and prettyPrintBRightPrec (prec: int) (b: bexpr) : string =
    if bprec b <= prec then sprintf "(%s)" (prettyPrintB b)
    else prettyPrintB b

and prettyPrintBAtom (b: bexpr) : string =
    match b with
    | BTrue | BFalse | BNot _ -> prettyPrintB b
    | BEq _ | BNeq _ | BGt _ | BGe _ | BLt _ | BLe _ -> prettyPrintB b
    | _ -> sprintf "(%s)" (prettyPrintB b)

let rec prettyPrintC (c: command) : string =
    prettyPrintCIndent 0 c

and prettyPrintCIndent (indent: int) (c: command) : string =
    let pad = String.replicate indent "   "
    match c with
    | CAssign(x, a) -> sprintf "%s%s := %s" pad x (prettyPrintA a)
    | CAAssign(name, idx, a) -> sprintf "%s%s[%s] := %s" pad name (prettyPrintA idx) (prettyPrintA a)
    | CSkip -> sprintf "%sskip" pad
    | CSeq(c1, c2) -> sprintf "%s ;\n%s" (prettyPrintCIndent indent c1) (prettyPrintCIndent indent c2)
    | CIf gc -> sprintf "%sif %s%sfi" pad (prettyPrintGC (indent + 1) gc) pad
    | CDo gc -> sprintf "%sdo %s%sod" pad (prettyPrintGC (indent + 1) gc) pad

and prettyPrintGC (indent: int) (gc: guardedcommand) : string =
    match gc with
    | GCGuard(b, c) -> sprintf "%s ->\n%s\n" (prettyPrintB b) (prettyPrintCIndent indent c)
    | GCChoice(gc1, gc2) -> sprintf "%s[] %s" (prettyPrintGC indent gc1) (prettyPrintGC indent gc2)

let prettyPrint (ast: command) : string =
    prettyPrintC ast

let analysis (input: Input) : Output =
    match parse Grammar.start_commands input.commands with
        | Ok ast ->
            Console.Error.WriteLine("> {0}", ast)
            { pretty = prettyPrint ast }
        | Error e -> { pretty = String.Format("Parse error: {0}", e) }
