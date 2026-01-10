module Configuration

open System

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
        "sqrt", (fun (a: float[]) -> Math.Sqrt a.[0])
        "abs", (fun (a: float[]) -> Math.Abs a.[0])
        "max", (fun (a: float[]) -> Array.max a)
        "min", (fun (a: float[]) -> Array.min a)
    ]