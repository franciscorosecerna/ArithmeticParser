module Evaluator

open System
open Ast
open Configuration
open Common

let rec eval (expr: Expr) : float =
    match expr with
    | Num value ->
        value

    | ConstantRef name ->
        match constants.TryGetValue name with
        | true, v -> v
        | _ -> raise (EvaluationException $"Unknown constant: {name}")

    | Unary (op, operand) ->
        let v = eval operand
        match op with
        | '-' -> -v
        | '+' -> v
        | _ ->
            raise (EvaluationException $"Unary operator unknown: {op}")

    | Binary (left, op, right) ->
        let l = eval left
        let r = eval right
        match op with
        | '+' -> l + r
        | '-' -> l - r
        | '*' -> l * r
        | '/' ->
            if abs r < Double.Epsilon then
                raise (EvaluationException "Division by zero")
            l / r
        | '^' ->
            Math.Pow(l, r)
        | _ ->
            raise (EvaluationException $"Binary operator unknown: {op}")

    | Call (funcName, args) ->
        match functions.TryGetValue funcName with
        | true, f ->
            let values = args |> List.map eval |> List.toArray
            f values
        | _ ->
            raise (EvaluationException $"Unknown function: {funcName}")