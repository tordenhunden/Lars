
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


#load "Incom.Cols.fs"

open Incom
open System.IO




[<Literal>]
let path = __SOURCE_DIRECTORY__ + "/SalesJan2009.csv" 

let content = File.ReadAllLines path


let zs = 
  content 
  |> Array.map (fun x -> x.Split(';'))
  |> Array.map (fun x -> {| TransactionDate  = x.[0]
                            Product          = x.[1]
                            Price            = x.[2]
                            PaymentType      = x.[3]
                            Name             = x.[4]
                            City             = x.[5]
                            State            = x.[6]
                            Country          = x.[7]
                            AccountCreated   = x.[8]
                            LastLogin        = x.[9]
                            Latitude         = x.[10]
                            Longitude        = x.[11] |})
  |> Array.filter (fun x -> x.PaymentType = "Visa")
  |> Array.sortBy (fun x -> x.State)

zs |> Cols.ofProperties true |> Cols.align 1000 |> Cols.print " | "






//alternative
type Csv = CsvProvider<path,Separators=";">

let parsed = Csv.Load path
let rows = 
  parsed.Rows 
  |> Seq.toArray 
  |> Array.filter (fun x -> x.State = "NY")


Cols.ofCsvTypeProviderRows parsed.Headers parsed.Rows |> Cols.align 1000 |> Cols.print " | "
