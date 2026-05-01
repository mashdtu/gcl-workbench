module BiGCL
open Io.BiGCL
open AST
open Parser

let counter = ref 0
let usedVars = ref Set.empty<string>

let nexttem () =
    let mutable n = counter.Value
    let mutable name = "tmp" + string n + "_"
    while Set.contains name usedVars.Value do
        n <- n + 1
        name <- "tmp" + string n + "_"
    counter.Value <- n + 1
    name

let rec collectVarsA a =
    match a with
    | AVar v              -> Set.singleton v
    | AArray(v, idx)      -> Set.add v (collectVarsA idx)
    | AUMinus a1          -> collectVarsA a1
    | APlus(a1,a2)|AMinus(a1,a2)|ATimes(a1,a2)|ADiv(a1,a2)|APow(a1,a2) ->
        Set.union (collectVarsA a1) (collectVarsA a2)
    | _ -> Set.empty

let rec collectVarsB b =
    match b with
    | BNot b1             -> collectVarsB b1
    | BAnd(b1,b2)|BOr(b1,b2)|BSAnd(b1,b2)|BSOr(b1,b2) ->
        Set.union (collectVarsB b1) (collectVarsB b2)
    | BEq(a1,a2)|BNeq(a1,a2)|BGt(a1,a2)|BGe(a1,a2)|BLt(a1,a2)|BLe(a1,a2) ->
        Set.union (collectVarsA a1) (collectVarsA a2)
    | _ -> Set.empty

let rec collectVarsC c =
    match c with
    | CAssign(x, a)   -> Set.add x (collectVarsA a)
    | CAAssign(x, i, a) -> Set.add x (Set.union (collectVarsA i) (collectVarsA a))
    | CSeq(c1, c2)    -> Set.union (collectVarsC c1) (collectVarsC c2)
    | CIf gc | CDo gc -> collectVarsGC gc
    | _ -> Set.empty

and collectVarsGC gc =
    match gc with
    | GCGuard(b, c)    -> Set.union (collectVarsB b) (collectVarsC c)
    | GCChoice(g1, g2) -> Set.union (collectVarsGC g1) (collectVarsGC g2)

// Chain a list of commands, dropping CSkips
let rec seqOf cmds =
    match List.filter (fun c -> c <> CSkip) cmds with
    | []       -> CSkip
    | [c]      -> c
    | c :: rest -> CSeq(c, seqOf rest)

// ── Arithmetic simplification ─────────────────────────────────────────────────

let rec isSimple a =
    match a with
    | ANum _ | AVar _ -> true
    | _ -> false

// Assign 'a' to a temp variable 'x'. 'x' may receive a binary op directly
// when both operands are already simple; otherwise subexpressions are reduced
// into further temps first.
let rec getCAssignTemp x a =
    match a with
    | ANum n              -> CAssign(x, ANum n)
    | AVar v              -> CAssign(x, AVar v)
    | AUMinus(AVar v)     -> CAssign(x, AUMinus(AVar v))
    | AUMinus(ANum n)     -> CAssign(x, AUMinus(ANum n))
    | AUMinus a1          -> let t = nexttem() in seqOf [getCAssignTemp t a1; CAssign(x, AUMinus(AVar t))]
    | APlus(n1, n2) when isSimple n1 && isSimple n2 -> CAssign(x, APlus(n1, n2))
    | APlus(n1, n2) when isSimple n1 ->
        let t = nexttem() in seqOf [getCAssignTemp t n2; getCAssignTemp x (APlus(n1, AVar t))]
    | APlus(n1, n2) ->
        let t = nexttem() in seqOf [getCAssignTemp t n1; getCAssignTemp x (APlus(AVar t, n2))]
    | AMinus(n1, n2) when isSimple n1 && isSimple n2 -> CAssign(x, AMinus(n1, n2))
    | AMinus(n1, n2) when isSimple n1 ->
        let t = nexttem() in seqOf [getCAssignTemp t n2; getCAssignTemp x (AMinus(n1, AVar t))]
    | AMinus(n1, n2) ->
        let t = nexttem() in seqOf [getCAssignTemp t n1; getCAssignTemp x (AMinus(AVar t, n2))]
    | ATimes(n1, n2) when isSimple n1 && isSimple n2 -> CAssign(x, ATimes(n1, n2))
    | ATimes(n1, n2) when isSimple n1 ->
        let t = nexttem() in seqOf [getCAssignTemp t n2; getCAssignTemp x (ATimes(n1, AVar t))]
    | ATimes(n1, n2) ->
        let t = nexttem() in seqOf [getCAssignTemp t n1; getCAssignTemp x (ATimes(AVar t, n2))]
    | ADiv(n1, n2) when isSimple n1 && isSimple n2 -> CAssign(x, ADiv(n1, n2))
    | ADiv(n1, n2) when isSimple n1 ->
        let t = nexttem() in seqOf [getCAssignTemp t n2; getCAssignTemp x (ADiv(n1, AVar t))]
    | ADiv(n1, n2) ->
        let t = nexttem() in seqOf [getCAssignTemp t n1; getCAssignTemp x (ADiv(AVar t, n2))]
    | APow(n1, n2) when isSimple n1 && isSimple n2 -> CAssign(x, APow(n1, n2))
    | APow(n1, n2) when isSimple n1 ->
        let t = nexttem() in seqOf [getCAssignTemp t n2; getCAssignTemp x (APow(n1, AVar t))]
    | APow(n1, n2) ->
        let t = nexttem() in seqOf [getCAssignTemp t n1; getCAssignTemp x (APow(AVar t, n2))]
    | _ -> CAssign(x, AVar "missing")

// Assign 'a' to any variable 'x'. Guarantees 'x' is only ever given a simple
// RHS (AVar / ANum / AUMinus(simple)); all other expressions are first
// evaluated into a fresh temp and then copied to 'x'.
let getCAssign x a =
    match a with
    | AVar _ | ANum _ | AUMinus(AVar _) | AUMinus(ANum _) -> CAssign(x, a)
    | _ ->
        let t = nexttem()
        seqOf [getCAssignTemp t a; CAssign(x, AVar t)]

// ── Boolean materialisation ───────────────────────────────────────────────────

// Ensure a boolean operand is just AVar or ANum (move complex exprs into a temp)
let simplifyComp a =
    match a with
    | AVar _ | ANum _ -> (a, CSkip)
    | _ ->
        let t = nexttem()
        (AVar t, getCAssignTemp t a)

// Rebuild a comparison with new operands
let rebuildComp b sa1 sa2 =
    match b with
    | BEq  _ -> BEq (sa1, sa2)
    | BNeq _ -> BNeq(sa1, sa2)
    | BGt  _ -> BGt (sa1, sa2)
    | BGe  _ -> BGe (sa1, sa2)
    | BLt  _ -> BLt (sa1, sa2)
    | BLe  _ -> BLe (sa1, sa2)
    | _      -> b

let getCompOps b =
    match b with
    | BEq(a1,a2)|BNeq(a1,a2)|BGt(a1,a2)|BGe(a1,a2)|BLt(a1,a2)|BLe(a1,a2) -> (a1, a2)
    | _ -> failwith "not a comparison"

// Compile bexpr → (flagVar, setupCode)
// After setupCode, flagVar = 1 if b was true, 0 if false
let rec compileBool b =
    match b with
    | BTrue ->
        let t = nexttem()
        (t, CAssign(t, ANum 1))
    | BFalse ->
        let t = nexttem()
        (t, CAssign(t, ANum 0))
    | BNot b1 ->
        let (t1, code1) = compileBool b1
        (t1, seqOf [code1; CAssign(t1, AMinus(ANum 1, AVar t1))])
    | BAnd(b1, b2) | BSAnd(b1, b2) ->
        let (t1, code1) = compileBool b1
        let (t2, code2) = compileBool b2
        let t = nexttem()
        (t, seqOf [code1; code2; CAssign(t, ATimes(AVar t1, AVar t2))])
    | BOr(b1, b2) | BSOr(b1, b2) ->
        let (t1, code1) = compileBool b1
        let (t2, code2) = compileBool b2
        let t = nexttem()
        let ifOr = CIf(GCChoice(
            GCGuard(BEq(AVar t1, ANum 1), CAssign(t, ANum 1)),
            GCGuard(BNot(BEq(AVar t1, ANum 1)), seqOf [code2; CAssign(t, AVar t2)])
        ))
        (t, seqOf [code1; ifOr])
    | BEq _ | BNeq _ | BGt _ | BGe _ | BLt _ | BLe _ ->
        let (a1, a2) = getCompOps b
        let t  = nexttem()               // allocate flag first
        let (sa1, ca1) = simplifyComp a1 // then computation temps
        let (sa2, ca2) = simplifyComp a2
        let b' = rebuildComp b sa1 sa2
        (t, seqOf [ca1; ca2;
                   CIf(GCChoice(GCGuard(b', CAssign(t, ANum 1)),
                                GCGuard(BNot b', CAssign(t, ANum 0))))])

// ── Command transformation ────────────────────────────────────────────────────

let flattenGC gc =
    let rec go gc =
        match gc with
        | GCGuard(b, c)    -> [(b, c)]
        | GCChoice(g1, g2) -> go g1 @ go g2
    go gc

let stuck = CAssign("stuck_", ADiv(ANum 1, ANum 0))

// True when a boolean can be used as a guard directly (no temp needed)
let isSimpleBool b =
    match b with
    | BTrue | BFalse -> true
    | BEq(a1,a2)|BNeq(a1,a2)|BGt(a1,a2)|BGe(a1,a2)|BLt(a1,a2)|BLe(a1,a2) ->
        isSimple a1 && isSimple a2
    | BNot(BEq(a1,a2))|BNot(BNeq(a1,a2))|BNot(BGt(a1,a2))|BNot(BGe(a1,a2))|BNot(BLt(a1,a2))|BNot(BLe(a1,a2)) ->
        isSimple a1 && isSimple a2
    | _ -> false

// Nested binary if: each guard materialises to a flag, falls through to next
// If the guard condition is already simple, skip materialization entirely
let rec buildIf guards =
    match guards with
    | [] -> stuck
    | (b, c) :: rest ->
        if isSimpleBool b then
            CIf(GCChoice(
                GCGuard(b, makecommand c),
                GCGuard(BNot b, buildIf rest)
            ))
        else
            let (t, setup) = compileBool b
            let guard = BEq(AVar t, ANum 1)
            seqOf [setup; CIf(GCChoice(
                GCGuard(guard, makecommand c),
                GCGuard(BNot guard, buildIf rest)
            ))]

// Do loop: single guard materialises; recomputed at end of each iteration
and buildDo guards =
    match guards with
    | [(b, c)] ->
        let (t, setup) = compileBool b
        let guard = BEq(AVar t, ANum 1)
        let body = seqOf [makecommand c; setup]
        seqOf [setup; CDo(GCGuard(guard, body))]
    | _ ->
        // Multi-guard: OR all guards for loop condition; nested if in body selects branch
        let bCombined = guards |> List.map fst |> List.reduce (fun a b -> BOr(a, b))
        let (t, setup) = compileBool bCombined
        let guard = BEq(AVar t, ANum 1)
        let body = seqOf [buildIf guards; setup]
        seqOf [setup; CDo(GCGuard(guard, body))]

and makecommand cmd =
    match cmd with
    | CSkip           -> CSkip
    | CAssign(x, a)   -> getCAssign x a
    | CSeq(c1, c2)    -> CSeq(makecommand c1, makecommand c2)
    | CIf gc          -> buildIf (flattenGC gc)
    | CDo gc          -> buildDo (flattenGC gc)
    | _               -> CSkip

// ── RiscV-flavoured variants: user vars may receive (simple binop simple) directly ──

and buildIfRV guards =
    match guards with
    | [] -> stuck
    | (b, c) :: rest ->
        if isSimpleBool b then
            CIf(GCChoice(
                GCGuard(b, makecommandRV c),
                GCGuard(BNot b, buildIfRV rest)
            ))
        else
            let (t, setup) = compileBool b
            let guard = BEq(AVar t, ANum 1)
            seqOf [setup; CIf(GCChoice(
                GCGuard(guard, makecommandRV c),
                GCGuard(BNot guard, buildIfRV rest)
            ))]

and buildDoRV guards =
    match guards with
    | [(b, c)] ->
        let (t, setup) = compileBool b
        let guard = BEq(AVar t, ANum 1)
        let body = seqOf [makecommandRV c; setup]
        seqOf [setup; CDo(GCGuard(guard, body))]
    | _ ->
        let bCombined = guards |> List.map fst |> List.reduce (fun a b -> BOr(a, b))
        let (t, setup) = compileBool bCombined
        let guard = BEq(AVar t, ANum 1)
        let body = seqOf [buildIfRV guards; setup]
        seqOf [setup; CDo(GCGuard(guard, body))]

and makecommandRV cmd =
    match cmd with
    | CSkip           -> CSkip
    | CAssign(x, a)   -> getCAssignTemp x a   // permissive: binop(simple,simple) stays on user var
    | CSeq(c1, c2)    -> CSeq(makecommandRV c1, makecommandRV c2)
    | CIf gc          -> buildIfRV (flattenGC gc)
    | CDo gc          -> buildDoRV (flattenGC gc)
    | _               -> CSkip

let getbinary x = makecommand x

// RiscV entry point: initializes name-collision state, uses permissive transformation
let getbinaryRV ast =
    counter.Value <- 0
    usedVars.Value <- collectVarsC ast
    makecommandRV ast

let analysis (input: Input) : Output =
    counter.Value <- 0
    match parse Grammar.start_commands input.commands with
    | Ok ast  ->
        usedVars.Value <- collectVarsC ast
        { binary = prettyPrint (getbinary ast) }
    | Error _ -> { binary = "" }

 

