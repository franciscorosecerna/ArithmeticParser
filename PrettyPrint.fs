module PrettyPrint

open System.Globalization
open Ast
open Configuration

let rec prettyPrint (parentPrec: int) (expr: Expr) : string =
    match expr with
    | Num v ->
        v.ToString(CultureInfo.InvariantCulture)
        
    | Variable name ->
        name
        
    | ConstantRef name ->
        name
        
    | Unary (op, operand) ->
        let exprPrec = unaryPrecedence
        let inner = prettyPrint exprPrec operand
        let text = string op + inner
        if exprPrec < parentPrec then $"({text})" else text
        
    | Binary (left, op, right) ->
        let (prec, rightAssoc) = operatorConfig[op]
        let leftNeedsParens =
            match left with
            | Binary(_, lop, _) ->
                let (lp, _) = operatorConfig[lop]
                lp < prec || (lp = prec && rightAssoc)
            | _ -> false
        let rightNeedsParens =
            match right with
            | Binary(_, rop, _) ->
                let (rp, _) = operatorConfig[rop]
                rp < prec || (rp = prec && not rightAssoc)
            | _ -> false
        let leftStr =
            let s = prettyPrint prec left
            if leftNeedsParens then $"({s})" else s
        let rightStr =
            let s = prettyPrint prec right
            if rightNeedsParens then $"({s})" else s
        let text = $"{leftStr} {op} {rightStr}"
        if prec < parentPrec then $"({text})" else text
        
    | Call (name, args) ->
        let argsStr =
            args
            |> List.map (prettyPrint 0)
            |> String.concat ", "
        $"{name}({argsStr})"