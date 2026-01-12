module Simplifier

open Ast

let rec simplify expr =
    match expr with
    | Num _ -> expr
    | Variable _ -> expr
    | ConstantRef _ -> expr
    
    | Unary (op, e) ->
        let e' = simplify e
        match op, e' with
        | '-', Num v -> Num (-v)
        | '-', Unary('-', inner) -> inner
        | _ -> Unary(op, e')
        
    | Binary (l, op, r) ->
        let l' = simplify l
        let r' = simplify r
        match op, l', r' with
        | '+', Num a, Num b -> Num (a + b)
        | '-', Num a, Num b -> Num (a - b)
        | '*', Num a, Num b -> Num (a * b)
        | '/', Num a, Num b -> Num (a / b)
        | '^', Num a, Num b -> Num (a ** b)
        
        | '+', x, Num 0.0 -> x
        | '+', Num 0.0, x -> x
        
        | '-', x, Num 0.0 -> x
        | '-', Num 0.0, x -> Unary('-', x)

        | '*', x, Num 1.0 -> x
        | '*', Num 1.0, x -> x
        | '*', _, Num 0.0 -> Num 0.0
        | '*', Num 0.0, _ -> Num 0.0
        
        | '/', x, Num 1.0 -> x
        | '/', Num 0.0, _ -> Num 0.0
        
        | '^', x, Num 0.0 -> Num 1.0
        | '^', x, Num 1.0 -> x
        | '^', Num 1.0, _ -> Num 1.0
        
        | _ -> Binary(l', op, r')
        
    | Call (name, args) ->
        let args' = args |> List.map simplify
        Call(name, args')