module Evaluator

open System
open Ast
open Configuration
open Common

let rec eval (env: Map<string, float>) (expr: Expr) : Expr =
    match expr with
    | Num _ -> expr
    
    | Variable name ->
        match Map.tryFind name env with
        | Some value -> Num value
        | None -> expr
    
    | ConstantRef name ->
        match constants.TryGetValue name with
        | true, v -> Num v
        | _ -> raise (EvaluationException $"Unknown constant: {name}")
    
    | Unary (op, operand) ->
        let e' = eval env operand
        match op, e' with
        | '-', Num v -> Num (-v)
        | '+', Num v -> Num v
        | '-', Unary('-', inner) -> inner
        | _ -> Unary(op, e')
    
    | Binary (left, op, right) ->
        let l' = eval env left
        let r' = eval env right
        
        match l', r' with
        | Num a, Num b ->
            let result =
                match op with
                | '+' -> a + b
                | '-' -> a - b
                | '*' -> a * b
                | '/' ->
                    if abs b < Double.Epsilon then
                        raise (EvaluationException "Division by zero")
                    a / b
                | '^' -> Math.Pow(a, b)
                | _ -> raise (EvaluationException $"Binary operator unknown: {op}")
            
            if Double.IsNaN result then
                raise (EvaluationException "Result is NaN")
            elif Double.IsInfinity result then
                raise (EvaluationException "Result is infinite")
            else
                Num result

        | Num 0.0, x | x, Num 0.0 when op = '+' -> x
        | x, Num 0.0 when op = '-' -> x
        | Num 0.0, x when op = '-' -> Unary('-', x)
        | Num 1.0, x | x, Num 1.0 when op = '*' -> x
        | Num 0.0, _ | _, Num 0.0 when op = '*' -> Num 0.0
        | x, Num 1.0 when op = '/' -> x
        | Num 0.0, _ when op = '/' -> Num 0.0
        | x, Num 0.0 when op = '^' -> Num 1.0
        | x, Num 1.0 when op = '^' -> x
        | Num 1.0, _ when op = '^' -> Num 1.0
        
        | _ -> Binary(l', op, r')
    
    | Call (name, args) ->
        let args' = args |> List.map (eval env)

        let allNumbers =
            args' |> List.forall (function | Num _ -> true | _ -> false)
        
        if allNumbers then
            match functions.TryGetValue name with
            | true, f ->
                let values =
                    args'
                    |> List.map (function | Num v -> v | _ -> failwith "unreachable")
                    |> List.toArray
                
                let result = f values
                
                if Double.IsNaN result then
                    raise (EvaluationException "Result is NaN")
                elif Double.IsInfinity result then
                    raise (EvaluationException "Result is infinite")
                else
                    Num result
            | _ ->
                raise (EvaluationException $"Unknown function: {name}")
        else
            Call(name, args')

let toFloat (expr: Expr) : float option =
    match expr with
    | Num v -> Some v
    | _ -> None

let rec isFullyEvaluated (expr: Expr) : bool =
    match expr with
    | Num _ -> true
    | Variable _ -> false
    | ConstantRef _ -> false
    | Unary (_, e) -> isFullyEvaluated e
    | Binary (l, _, r) -> isFullyEvaluated l && isFullyEvaluated r
    | Call (_, args) -> args |> List.forall isFullyEvaluated

let evalNoVars (expr: Expr) : float =
    match eval Map.empty expr with
    | Num v -> v
    | _ -> raise (EvaluationException "Expression contains undefined variables")