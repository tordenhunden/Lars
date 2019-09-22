namespace Incom

open System
open Microsoft.FSharp.Reflection

module private DynamicCols =
  open Microsoft.FSharp.Quotations.Patterns

  let private getEvaluatorPropGet varName ex =
    match ex with
    | PropertyGet (Some ex, prop, _) ->
      match ex with
      | Var var ->
        if var.Name = varName then
          fun obj -> prop.GetValue obj
        else
          failwithf "Var '%s' not bound. Available = ['%s']" var.Name varName
      | x -> 
        failwithf "Unsupported expression. Not a var: %A"x
    | x ->
      failwithf "Unsupported expression. Not a simple propertyAccessor: %A" x

  let getEvaluatorTupProjFunc ex =
    match ex with 
    | Lambda (var,body) ->
      match body with
      | NewTuple (es) ->
        let evaluators = es |> List.map (fun e -> getEvaluatorPropGet var.Name e) |> List.toArray
        fun x -> evaluators |> Array.map (fun eval-> eval x)
      | x ->
        failwithf "Unsupported expression. Not a tuple: %A" x
    | x ->
      failwithf "Unsupported expression. Not a lambda: %A" x


  let private labelFromPropGet ex =
    match ex with
    | PropertyGet (_, prop, _) ->
      prop.Name
    | x ->
      failwithf "Unsupported expression. Not a simple propertyAccessor: %A" x

  let labelsFromTupProjFunc ex =
    match ex with 
    | Lambda (_,body) ->
      match body with
      | NewTuple (es) ->
        es |> List.map labelFromPropGet |> List.toArray
      | x ->
        failwithf "Unsupported expression. Not a tuple: %A" x
    | x ->
      failwithf "Unsupported expression. Not a lambda: %A" x

[<RequireQualifiedAccess>]
module Cols =

  //assuming all rows have same col-count as header
  let private findMaxWidths (lookahead:int) (xs:string[] seq) =
    let folder (acc:int[]) (arr:string[]) =
      acc |> Array.mapi (fun i maxL -> Math.Max (maxL,arr.[i].Length))

    match Seq.tryHead xs with
    | None -> None
    | Some head -> 
      let init = head |> Array.map String.length
      let r = xs |> Seq.truncate lookahead |> Seq.fold folder init
      Some r

  let private objToString =
    function null -> "NULL" | x -> x.ToString()


  let alignWidths (widths:int[]) (xss:string[] seq) =
    xss |> Seq.map (fun xs -> (widths,xs) ||> Array.map2 (sprintf "%*s"))
  
  ///Assuming all rows have same col-count as header
  let align lookahead (xss:string[] seq) =
    match findMaxWidths lookahead xss with
    | None -> 
      xss
    | Some widths ->
      alignWidths widths xss

  let print sep (xss:string[] seq) =
    xss |> Seq.map (String.concat sep) |> (Seq.iter (printfn "%s"))

  let sprint sep (xss:string[] seq) =
    xss |> Seq.map (String.concat sep) |> String.concat "\n"

  let addLineNumbers (xss:string[] seq) =
    xss |> Seq.mapi (fun i xs -> Array.append [|sprintf "%i" i|] xs)

  let normalize (rows:string[] seq) =
    let normalize1 maxLength (xs:string[]) =
      if xs.Length < maxLength 
      then 
        let arr = Array.create maxLength ""
        Array.Copy (xs,arr,xs.Length)
        arr
      else xs
    let maxLength = rows |> Seq.map (fun x -> x.Length) |> Seq.max
    rows |> Seq.map (normalize1 maxLength)

  let ofTuples (xs:#obj seq) =
    xs |> Seq.map (FSharpValue.GetTupleFields >> Array.map objToString)

  let ofRecords (xs:#obj seq) =
    xs |> Seq.map (FSharpValue.GetRecordFields >> Array.map objToString)

  let ofProperties<'a when 'a :> obj> (includeHeader:bool) (xs:'a seq) =
    let t = typeof<'a>
    let props = t.GetProperties()
    let names = props |> Array.map (fun x -> x.Name)
    let readers = props |> Array.map (fun x obj -> x.GetValue obj)
  
    seq {
      if includeHeader then 
        yield names
      yield! xs |> Seq.map (fun obj -> readers |> Array.map (fun reader -> reader obj |> objToString))
    }

  ///expression format: <@ fun x -> x.Field1,X.Field2 @> (Must be a tuple. Only supports property getters)
  let ofExpression (makeHeader:bool) (ex:Quotations.Expr<'a -> _>) (xs: seq<'a>) =
    seq {
      if makeHeader then
        yield DynamicCols.labelsFromTupProjFunc ex |> Seq.toArray
      let evaluator = DynamicCols.getEvaluatorTupProjFunc ex

      yield! xs |> Seq.map (evaluator >> Array.map objToString)
    }

  let ofCsvTypeProviderRows maybeHeaders rows =
    seq {
      match maybeHeaders with 
      | None -> ()
      | Some headers -> yield headers

      yield! rows |> Seq.map (FSharpValue.GetTupleFields >> Array.map objToString)
    }

  let ofCsvString (sep:char) (csv:string) = 
    csv.Split ([|'\n';'\r'|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun x -> x.Split sep)
  

  let filterCols (p:string->bool) (xs:string[] seq) =
    let includeHeader = xs |> Seq.head |> Array.map p
    xs |> Seq.map (Array.zip includeHeader) |> Seq.map (Array.filter fst >> Array.map snd)

  let addHeader (xs:string[]) (xss:string[] seq) =
    Seq.append [xs] xss

  ///assumes that there is a header
  let toDeedleValues (xss:string[] seq) =
    match xss |> Seq.tryHead with
    | None -> failwith "Must have a header. Also, there is no data"
    | Some header -> 
      xss |> Seq.tail |> Seq.mapi (fun rowI xs -> header |> Seq.mapi (fun colI c -> rowI,c,xs.[colI])) |> Seq.collect id


  /// (rowKey * colKey * val) seq
  let ofValues keyColName (values:(string*string*string) seq) =
    let map = 
      values 
      |> Seq.groupBy (fun (r,c,_) -> r,c) 
      |> Seq.map (fun (k,xs) -> k, xs |> Seq.map (fun (_,_,v) -> v) |> Seq.exactlyOne) 
      |> Map.ofSeq

    let getVal r c =
      match map.TryFind (r,c) with 
      | None -> failwithf "No value for %A" (r,c) 
      | Some v -> v
    let makeRow cols r =
      seq {
        yield r
        for c in cols do
          yield getVal r c
      } |> Seq.toArray

    let rows = values |> Seq.map (fun (r,_,_) -> r) |> Seq.distinct
    let cols = values |> Seq.map (fun (_,c,_) -> c) |> Seq.distinct |> Seq.toArray
  
    seq {
      yield Seq.append [keyColName] cols |> Seq.toArray
      yield! rows |> Seq.map (makeRow cols)
    }
