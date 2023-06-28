namespace AdventOfCode

open System

module Days = 

   open System.IO
   
   //day 1 methods
   let splitByBlankLine (data: string List) =
      let rec inner (lists: string list list) (rest: string List) =
         if rest = [] then lists
         else
            let index = List.tryFindIndex String.IsNullOrWhiteSpace rest
            match index with
               | Some i ->
                  let temp, newRest =
                     if i = 0 then
                        List.tail rest, List.tail rest
                     else
                        List.takeWhile (fun i -> not (i = "")) rest, List.removeManyAt 0 (i+1) rest
                  inner (temp::lists) newRest
               | None ->
                  let final = rest
                  inner (final::lists) []
      inner List.empty data
   
 
   let findElf (elves: string list list) =
      elves
      |> List.map (List.filter (fun i -> not (i = "")))
      |> List.map (List.map int)
      |> List.map List.sum 
      |> List.sortDescending
      |> List.truncate 3
      |> List.sum
   
   //day 2 methods
   let playScore = dict ["X", 1; "Y", 2; "Z",3]
   let resultScore = dict ["W", 6; "D", 3; "L",0]
   let conversions = dict ["A", "X"; "B", "Y"; "C","Z"]
   let winningResults = dict ["A", "Y"; "B", "Z"; "C", "X"] 
   let losingResults = dict ["A", "Z"; "B", "X"; "C", "Y"] 

       
   let checkResult a b =
      match a, b with
      | x, y when (conversions[x] = y) -> resultScore["D"]
      | _, _ ->
         if winningResults[a] = b then resultScore["W"]
         else resultScore["L"]
   
   let calcScore rows =
      
      let rec inner (rows: string list) score =
         if rows = [] then score
         else
            let row = List.head rows
            let plays = row.Split " " |> Array.toList |> List.map (fun i -> i.Trim())
            let roundScore =
                  let a = playScore[plays[1]]
                  let b = checkResult plays[0] plays[1]
                  a + b
            inner (List.tail rows) (score + roundScore)
      
      inner rows 0       
   
   let getMove a b =
      let result =
         match a,b with
         | _, "Y" -> playScore[conversions[a]] + resultScore["D"]
         | _, "X" -> playScore[losingResults[a]] + resultScore["L"]
         | _, "Z" -> playScore[winningResults[a]] + resultScore["W"]
      result     
   
   let chooseMoves rows =
      let rec inner (rows: string list) score =
         if rows = [] then score
         else
            let row = List.head rows
            let plays = row.Split " " |> Array.toList |> List.map (fun i -> i.Trim())
            let roundScore = getMove plays[0] plays[1]      
            inner (List.tail rows) (score + roundScore)
      
      inner rows 0 
      
      
   let day1 v =
      let filePath = v
      let rows = File.ReadLines filePath |> Seq.toList
       
      let finalRows = rows |> splitByBlankLine |> List.rev
      
      let answer = findElf finalRows
      printfn $"{answer}"
   
   
   let day2 v =
      let filePath = v
      let rows = File.ReadLines filePath |> Seq.toList
      //let answer = calcScore rows
      let answer = chooseMoves rows 
      printfn $"{answer}"