module Program

open Tokenizer
open Parser
open Evaluator
open PrettyPrint

[<EntryPoint>]
let main _ =
    let expr = System.Console.ReadLine() |> Option.ofObj |> Option.defaultValue ""
    let ast = expr |> tokenize |> buildAst
    printfn "Pretty: %s" (prettyPrint 0 ast)
    printfn "Result: %.4f" (eval ast)
    0