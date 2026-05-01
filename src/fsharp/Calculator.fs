module Calculator
open Io.Calculator

open AST
open System



let rec evaluate (expr: expr) : Result<int, string> =
    match expr with
    | Num(x) -> Ok(x) 

    | TimesExpr(x,y) -> 
        match (evaluate x,evaluate y) with
        | (Ok(xx),Ok(yy)) -> Ok(xx*yy)
        | (_,_) -> fail "mis"

    | DivExpr (x,y) ->
        match (evaluate x,evaluate y) with
        | (_,Ok(0)) -> fail "div"
        | (Ok(xx),Ok(yy)) -> Ok(xx/yy)
        | (_,_) -> fail "mis"

    | PlusExpr (x,y) -> 
        match (evaluate x,evaluate y) with
        | (Ok(xx),Ok(yy)) -> Ok(xx+yy)
        | (_,_) -> fail "mis"

    | MinusExpr (x,y) -> 
        match (evaluate x,evaluate y) with
        | (Ok(xx),Ok(yy)) -> Ok(xx-yy)
        | (_,_) -> fail "mis"

    | PowExpr (x,y) -> 
        match (evaluate x,evaluate y) with
        | (Ok(xx),Ok(yy)) ->
            let res = power (bigint xx) yy
            if res < bigint System.Int32.MinValue || res > bigint System.Int32.MaxValue then
                Error "overflow"
            else
                Ok (int res)
        | (_,_) -> Error "expression evaluator not yet implemented"

    | UMinusExpr (x) -> 
        match evaluate x with
            | Ok(xx) ->
                if xx = System.Int32.MinValue then Error "overflow"
                else Ok(-xx)
            | _ -> Error "expression evaluator not yet implemented"
    
    | _ -> fail "mis"

and power (x: bigint) (y: int) : bigint =
    if y < 0 then failwith "power of negative is not allowed"
    else
        let rec powerA acc exp =
            if exp = 0 then acc
            else powerA (acc * x) (exp - 1)

        powerA 1I y

and fail (a) : Result<int, string> =
    match a with
    | "div" -> Error "Divided by 0 not allowed"
    | _ -> Error "expression evaluator not yet implemented"


let analysis (input: Input) : Output =
    match Parser.parse Grammar.start_expression input.expression with
    | Ok ast ->
        Console.Error.WriteLine("> {0}", ast)
        match evaluate ast with
        | Ok result -> { result = result.ToString(); error = "" }
        | Error e -> { result = ""; error = String.Format("Evaluation error: {0}", e) }
    | Error e -> { result = ""; error = String.Format("Parse error: {0}", e) }
