open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main argv : int =
    (* Check for proper usage *)
    if argv.Length <> 1 && argv.Length <> 2 then
        printfn "Usage: dotnet run <file> [debug] > [filename].svg"
        exit 1

    (* read in the input file *)
    let file = argv.[0]
    let input = File.ReadAllText file

    (* does the user want parser debugging turned on? *)
    let do_debug = if argv.Length = 2 then true else false

    (* try to parse what they gave us *)
    let ast_maybe = parse input do_debug
    match ast_maybe with
    | Some canvas_ast ->
        printfn "%s" (eval canvas_ast)
        0
    | _ ->
        printfn "Invalid program. \n Usage: dotnet run <file> > [filename].svg"
        1
    