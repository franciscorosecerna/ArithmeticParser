module Tokenizer

open System
open System.Globalization
open Tokens
open Common
open Configuration

let tokenize (input: string) : Token list =
    let text = input.Replace(" ", "")
    let tokens = ResizeArray<Token>()
    let mutable i = 0

    let isOperator c = operatorConfig.ContainsKey c

    let expectOperand () =
        tokens.Count = 0 ||
        (match tokens.[tokens.Count - 1] with
        | LeftParen
        | Operator _
        | UnaryOperator _
        | Comma -> true
        | _ -> false)

    while i < text.Length do
        let c = text.[i]

        if Char.IsDigit c || (c = '.' && i + 1 < text.Length && Char.IsDigit text.[i + 1]) then
            let start = i
            while i < text.Length && (Char.IsDigit text.[i] || text.[i] = '.') do
                i <- i + 1

            let numStr = text.Substring(start, i - start)

            match Double.TryParse(numStr, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, value ->
                tokens.Add(Number value)
            | _ ->
                raise (ParseException $"Invalid number: {numStr}")

        elif Char.IsLetter c then
            let start = i
            while i < text.Length && Char.IsLetterOrDigit text.[i] do
                i <- i + 1

            let ident = text.Substring(start, i - start)

            if i < text.Length && text.[i] = '(' then
                if functions.ContainsKey ident then
                    tokens.Add(Function ident)
                else
                    raise (ParseException $"Unknown function: {ident}")
            elif constants.ContainsKey ident then
                tokens.Add(Constant ident)
            else
                tokens.Add(Variable ident)

        else
            match c with
            | '(' ->
                tokens.Add LeftParen
                i <- i + 1

            | ')' ->
                tokens.Add RightParen
                i <- i + 1

            | ',' ->
                tokens.Add Comma
                i <- i + 1

            | _ when isOperator c ->
                if expectOperand() && (c = '-' || c = '+') then
                    tokens.Add(UnaryOperator(c, unaryPrecedence))
                else
                    let (prec, rightAssoc) = operatorConfig[c]
                    tokens.Add(Operator(c, prec, rightAssoc))
                i <- i + 1

            | _ ->
                raise (ParseException $"Invalid Char: {c}")

    tokens |> Seq.toList