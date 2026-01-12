module Program

open System
open Tokenizer
open Parser
open Evaluator
open PrettyPrint
open Ast

let rec collectVariables (expr: Expr) : Set<string> =
    match expr with
    | Variable name -> Set.singleton name
    | Unary (_, e) -> collectVariables e
    | Binary (l, _, r) -> Set.union (collectVariables l) (collectVariables r)
    | Call (_, args) -> args |> List.map collectVariables |> Set.unionMany
    | _ -> Set.empty

[<EntryPoint>]
let main _ =
    let mutable env = Map.empty
    let mutable running = true
    let mutable lastExpr : Expr option = None

    printfn "Commands:"
    printfn "  set x=5        - Define variable"
    printfn "  show vars      - Show defined variables"
    printfn "  clear          - Clear all variables"
    printfn "  exit           - Quit"
    printf "  eval           - Evaluate last expression"
    printfn ""
    
    while running do
        printf "> "
        let input = Console.ReadLine() |> Option.ofObj |> Option.defaultValue ""
        
        match input.Trim().ToLower() with
        | "exit" | "quit" ->
            running <- false
            
        | "show vars" ->
            if Map.isEmpty env then
                printfn "No variables defined"
            else
                printfn "Defined variables:"
                env |> Map.iter (fun k v -> printfn "  %s = %.4f" k v)

        | "eval" ->
            match lastExpr with
            | None ->
                printfn "No expression to evaluate"
            | Some ast ->
                try
                    let result = eval env ast

                    match toFloat result with
                    | Some value ->
                        printfn "Result: %.4f" value
                    | None ->
                        printfn "Simplified: %s" (prettyPrint 0 result)
                        let remainingVars = collectVariables result
                        printfn "Undefined variables: %s"
                            (String.concat ", " remainingVars)
                with
                | ex -> printfn "Error: %s" ex.Message 

        | "clear" ->
            env <- Map.empty
            printfn "All variables cleared"
            
        | cmd when cmd.StartsWith("set ") ->
            let assignment = cmd.Substring(4)
            match assignment.Split('=') with
            | [| name; value |] ->
                let varName = name.Trim()
                match Double.TryParse(value.Trim()) with
                | true, varValue ->
                    env <- Map.add varName varValue env
                    printfn "Set %s = %.4f" varName varValue
                | false, _ ->
                    printfn "Invalid number format"
            | _ -> printfn "Invalid format. Use: set x=5"
            
        | "" -> ()
        
        | expr ->
            try
                let ast = expr |> tokenize |> buildAst
                lastExpr <- Some ast

                printfn "Original: %s" (prettyPrint 0 ast)
                
                let result = eval env ast
                
                match toFloat result with
                | Some value ->
                    printfn "Result: %.4f" value
                | None ->
                    printfn "Simplified: %s" (prettyPrint 0 result)
                    let remainingVars = collectVariables result
                    printfn "Undefined variables: %s" (String.concat ", " remainingVars)
                    
            with
            | ex -> printfn "Error: %s" ex.Message
    
    0