module Parser

open Ast
open Tokens
open Common

let buildAst (tokens: Token list) : Expr =

    let tokens = List.toArray tokens
    let mutable pos = 0

    let peek () =
        if pos < tokens.Length then Some tokens[pos] else None

    let consume () =
        let t = tokens[pos]
        pos <- pos + 1
        t

    let expect token =
        match consume() with
        | t when t = token -> ()
        | _ ->
            match token with
            | RightParen -> raise (ParseException "Missing closing parenthesis")
            | _ -> raise (ParseException "Unexpected token")

    let rec parseExpression minPrec =
        let mutable left = parsePrefix()

        let rec loop () =
            match peek() with
            | Some (Operator (op, prec, rightAssoc)) when prec >= minPrec ->
                consume() |> ignore

                let nextMin =
                    if rightAssoc then prec else prec + 1

                let right = parseExpression nextMin
                left <- Binary(left, op, right)
                loop()

            | _ -> left

        loop()

    and parsePrefix () =
        match peek() with
        | Some (UnaryOperator (op, prec)) ->
            consume() |> ignore
            let expr = parseExpression (prec + 1)
            Unary(op, expr)

        | _ ->
            parsePrimary()

    and parsePrimary () =
        match consume() with
        | Number v ->
            Num v
        | Constant name ->
            ConstantRef name
        | Variable name ->
            Ast.Variable name
        | Function name ->
            expect LeftParen
            let args = parseArguments []
            Call(name, args)
        | LeftParen ->
            let expr = parseExpression 0
            expect RightParen
            expr
        | _ ->
            raise (ParseException $"Unexpected token at position {pos}")

    and parseArguments acc =
        match peek() with
        | None -> raise (ParseException "Missing closing parenthesis")
        | Some RightParen ->
            consume() |> ignore
            List.rev acc

        | _ ->
            let expr = parseExpression 0
            match peek() with
            | Some Comma ->
                consume() |> ignore
                parseArguments (expr :: acc)
            | Some RightParen ->
                consume() |> ignore
                List.rev (expr :: acc)
            | None -> raise (ParseException "Missing closing parenthesis")
            | _ ->
                raise (ParseException "Error in arguments parsing")

    let ast = parseExpression 0

    if pos < tokens.Length then
        raise (ParseException "Tokens remaining after parsing")

    Simplifier.simplify ast