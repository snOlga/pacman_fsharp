module Program
open Game
open System
open System.Text

[<EntryPoint>]
let main _ = 
    Console.OutputEncoding <- Encoding.Unicode
    initGame |> run
    0
