

let x = 3
let y = 2

let f x =
  3 + x

  

let add x y =
  x + y

let add_tup (x, y) =
  x + y

add_tup (2,3)

let add2 = add 2

add2 10;;



let str = "hej "

let hello name = str + name

hello "lars"

type IntTree =
  | Leaf of int
//  | LarsStringLeaf of string
  | Node of IntTree * IntTree
//  | NodeWithValue of int * IntTree * IntTree

let t1 = Leaf 2
let t2 = Node (Leaf 1,Leaf 2)
let t3 = Node (Leaf 1, Node (Leaf 2, Leaf 3))

let lookInside t = 
  match t with
  | Leaf i -> sprintf "Leaf with value %i" i
  | Node (t1,t2) -> "Node"

lookInside t1  
lookInside t2

let rec countLeaves t =
  match t with
  | Leaf _ -> 1
  | Node (t1,t2) -> 
    (countLeaves t1) + (countLeaves t2)

countLeaves t1
countLeaves t2

let rec sumLeaves t =
  match t with
  | Leaf i -> i
  | Node (t1,t2) -> 
    (sumLeaves t1) + (sumLeaves t2)

sumLeaves t1
sumLeaves t3

////



