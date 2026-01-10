module Configuration

open System
open Common

let operatorConfig =
    dict [
        '+', (2, false)
        '-', (2, false)
        '*', (3, false)
        '/', (3, false)
        '^', (5, true)
    ]

let unaryPrecedence = 4

let constants =
    dict [
        "pi", Math.PI
        "e", Math.E
        "tau", Math.PI * 2.0
    ]

let toRadians d = d * Math.PI / 180.0

let functions =
    dict<string, float[] -> float> [
        "sin", (fun (a: float[]) -> Math.Sin (toRadians a.[0]))
        "cos", (fun (a: float[]) -> Math.Cos (toRadians a.[0]))
        "tan", (fun (a: float[]) ->
            let cosv = Math.Cos (toRadians a.[0])
            if Math.Abs cosv < 1e-12 then
                raise (EvaluationException "Tangent undefined for angle")
            Math.Tan (toRadians a.[0]))
        "asin", (fun (a: float[]) ->
            if a.[0] < -1.0 || a.[0] > 1.0 then
                raise (EvaluationException "asin domain error: input must be in [-1,1]")
            Math.Asin a.[0] |> fun r -> r * 180.0 / Math.PI)
        "acos", (fun (a: float[]) ->
            if a.[0] < -1.0 || a.[0] > 1.0 then
                raise (EvaluationException "acos domain error: input must be in [-1,1]")
            Math.Acos a.[0] |> fun r -> r * 180.0 / Math.PI)
        "atan", (fun (a: float[]) -> Math.Atan a.[0] |> fun r -> r * 180.0 / Math.PI)
        "sqrt", (fun (a: float[]) ->
            if a.[0] < 0.0 then
                raise (EvaluationException "Square root of negative number")
            Math.Sqrt a.[0])
        "abs", (fun (a: float[]) -> Math.Abs a.[0])
        "max", (fun (a: float[]) ->
            if a.Length = 0 then
                raise (EvaluationException "max requires at least one argument")
            Array.max a)
        "min", (fun (a: float[]) ->
            if a.Length = 0 then
                raise (EvaluationException "min requires at least one argument")
            Array.min a)
        "ln",  (fun (a: float[]) ->
            if a.[0] <= 0.0 then
                raise (EvaluationException "Natural log of non-positive number")
            Math.Log a.[0])
        "log", (fun (a: float[]) ->
            if a.[0] <= 0.0 then
                raise (EvaluationException "Log10 of non-positive number")
            Math.Log10 a.[0])
    ]