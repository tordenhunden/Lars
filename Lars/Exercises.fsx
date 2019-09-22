


//Array.groupBy

fst (1,3)
snd (1,3)


let xs = 
  [| 
    1,"a"
    1,"b"
    2,"abc"
    2,"la"
    3,"x"
    1,"c"
  |]
xs 
|> Array.groupBy (fun (x,y) -> x) 
|> Array.map (fun (g,xs) -> g,xs |> Array.map snd)


//Lookup
//Array.find

let arrMap = 
  [|
    1,"k"
    2,"a"
    3,"h"
    //..
  |] 


let v = 
  let (key,value) = arrMap |> Array.find (fun (x,_) -> x = 1)
  value

//Map.find
let map = 
  [|
    1,"k"
    2,"a"
    3,"h"
    //..
  |] |> Map.ofArray


let value = 
  map |> Map.find 1

////


open System

let today = DateTime.Today
let random = Random()


let species = 
  [|
    "dog"
    "cat"
    "pig"
    "donkey"
    "horse"
    "snake"
  |]

let dates = 
  [|
    for i in 1 .. 100 do
      yield today.AddHours (float i)
  |]

let pickRandomElement (xs:_[]) =
  let i = random.Next xs.Length
  xs.[i]


//(date,species,kills)
let kills = 
  [|
    for i in  1 .. 10000 do
      yield (pickRandomElement dates, pickRandomElement species, random.Next 10)
  |] |> Array.sortBy (fun (x,_,_) -> x)

//points given for each kill
let points = 
  [|
    for i in  1 .. 10 do
      yield (pickRandomElement species,random.Next 10)
  |]



//find, for each datetime, how many points scored



//--ælajfdnæ