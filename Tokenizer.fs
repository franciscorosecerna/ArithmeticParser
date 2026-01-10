module Tokenizer

open System
open System.Globalization
open Tokens
open Common
open Configuration

let tokenize (input: string) : Token list =
    let len = input.Length

    let isOperator c =
        operatorConfig.ContainsKey c

    let rec readNumber i =
        let sb = Text.StringBuilder()
        let mutable j = i

        while j < len && (Char.IsDigit input[j] || input[j] = '.') do
            sb.Append(input[j]) |> ignore
            j <- j + 1

        let value =
            Double.Parse(sb.ToString(), CultureInfo.InvariantCulture)

        value, j

    let rec readIdentifier i =
        let sb = Text.StringBuilder()
        let mutable j = i

        while j < len && Char.IsLetter input[j] do
            sb.Append(input[j]) |> ignore
            j <- j + 1

        sb.ToString(), j

    let rec loop i prev acc =
        if i >= len then
            List.rev acc
        else
            let c = input[i]

            match c with
            | c when Char.IsWhiteSpace c ->
                loop (i + 1) prev acc

            | '(' ->
                loop (i + 1) (Some LeftParen) (LeftParen :: acc)

            | ')' ->
                loop (i + 1) (Some RightParen) (RightParen :: acc)

            | ',' ->
                loop (i + 1) (Some Comma) (Comma :: acc)

            | c when Char.IsDigit c || c = '.' ->
                let number, next = readNumber i
                loop next (Some (Number number)) (Number number :: acc)

            | c when Char.IsLetter c ->
                let ident, next = readIdentifier i

                let token =
                    if constants.ContainsKey ident then
                        Constant ident
                    elif functions.ContainsKey ident then
                        Function ident
                    else
                        raise (ParseException $"Unknown identifier: {ident}")

                loop next (Some token) (token :: acc)

            | c when isOperator c ->
                let isUnary =
                    match prev with
                    | None
                    | Some (Operator _)
                    | Some (UnaryOperator _)
                    | Some LeftParen
                    | Some Comma -> true
                    | _ -> false

                if isUnary then
                    loop (i + 1)
                         (Some (UnaryOperator (c, unaryPrecedence)))
                         (UnaryOperator (c, unaryPrecedence) :: acc)
                else
                    let (prec, rightAssoc) = operatorConfig[c]
                    loop (i + 1)
                         (Some (Operator (c, prec, rightAssoc)))
                         (Operator (c, prec, rightAssoc) :: acc)

            | _ ->
                raise (ParseException $"Invalid Char: '{c}'")

    loop 0 None [] |> ignore


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
                raise (ParseException $"Unknown identifier: {ident}")

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