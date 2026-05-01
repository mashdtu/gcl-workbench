// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module AST

type expr =
    | Num of int
    | TimesExpr of (expr * expr)
    | DivExpr of (expr * expr)
    | PlusExpr of (expr * expr)
    | MinusExpr of (expr * expr)
    | PowExpr of (expr * expr)
    | UMinusExpr of (expr)

// Arithmetic expressions
type aexpr =
    | ANum of bigint
    | AVar of string
    | AArray of (string * aexpr)
    | APlus of (aexpr * aexpr)
    | AMinus of (aexpr * aexpr)
    | ATimes of (aexpr * aexpr)
    | ADiv of (aexpr * aexpr)
    | APow of (aexpr * aexpr)
    | AUMinus of aexpr

// Boolean expressions
type bexpr =
    | BTrue
    | BFalse
    | BAnd of (bexpr * bexpr)
    | BOr of (bexpr * bexpr)
    | BSAnd of (bexpr * bexpr)
    | BSOr of (bexpr * bexpr)
    | BNot of bexpr
    | BEq of (aexpr * aexpr)
    | BNeq of (aexpr * aexpr)
    | BGt of (aexpr * aexpr)
    | BGe of (aexpr * aexpr)
    | BLt of (aexpr * aexpr)
    | BLe of (aexpr * aexpr)

// Commands
type command =
    | CAssign of (string * aexpr)
    | CAAssign of (string * aexpr * aexpr)
    | CSkip
    | CSeq of (command * command)
    | CIf of guardedcommand
    | CDo of guardedcommand

// Guarded commands
and guardedcommand =
    | GCGuard of (bexpr * command)
    | GCChoice of (guardedcommand * guardedcommand)
