// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

[<EntryPoint>]
let main argv = 
  Console.ReadLine().ToCharArray() |> Array.distinct |> String |> printfn "%s"
  0 // return an integer exit code
