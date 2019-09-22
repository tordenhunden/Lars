
let xs =
  [| 1;5;43;46;34;23;43;234 |]

let ys = xs |> Array.filter (fun x -> x > 20)

let isEven x = x % 2 = 0
let isOdd = isEven >> not

isEven 3
isOdd 2
let odds = xs |> Array.filter isOdd
let even = xs |> Array.filter isEven

let (evens',odds') = xs |> Array.partition isEven

let avg = 
  xs 
  |> Array.map decimal 
  |> Array.average

let xss = xs |> Array.chunkBySize 2

xss |> Array.collect id

let strings = 
  [| "lars"; "er";"blabla" |]

strings |> String.concat " | "

let largeArray = [| for i in 1..100000 do yield i |]
largeArray |> Array.map decimal |> Array.average

#r @"..\packages\FSharp.Data.3.2.4\lib\net45\FSharp.Data.dll"

open FSharp.Data

let world = WorldBankData.GetDataContext()
let elecAccess = world.Countries.Nigeria.Indicators.``Access to electricity (% of population)``



let years = elecAccess.Years |> Seq.toArray
let percentage = elecAccess.Values |> Seq.toArray
let zipped = Array.zip years percentage

let badYears =
  zipped 
  |> Array.pairwise 
  |> Array.filter (fun ((_,pX),(_,pY)) -> pX > pY) 
  |> Array.map (fun (_,(y,_))-> y)


let badYears' =
  zipped 
  |> Array.pairwise 
  |> Array.choose (fun ((_,pX),(y,pY)) -> if pX > pY then Some y else None) 
  






open System.Text.RegularExpressions




let tv2FrontPage = Http.RequestString "http://www.tv2.dk"
let collection = Regex.Matches (tv2FrontPage,"Kl\. \d\d\:\d\d.{10}",RegexOptions.Multiline)  
let matches = 
  seq {
    for i in collection do
      yield i.Value
  } |> Seq.toArray






