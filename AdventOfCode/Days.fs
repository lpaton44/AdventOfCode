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
                  if i = 0 then
                     inner lists (List.tail rest)
                  else
                     let temp = List.takeWhile (fun i -> not (i = "")) rest
                     let newRest = List.removeManyAt 0 (i + 1) rest
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
   
   
   //day 3 stuff
   let rec halveArray (a: 'a list) (b: 'a list) i =
      if i = 0 then a, b
      else
         halveArray (a @ [List.head b]) (List.tail b) (i - 1)
   
   let splitByThrees (data: string List) =                                       
      let rec inner (lists: string list list) (rest: string List) =                
         if rest = [] then lists                                                   
         else
            let newList = rest.[0..2]
            inner (newList::lists) rest.[3..]                                 
      inner List.empty data
   
   
   let rec checkInAll (a: char list)(b: char list list) =
      if a = List.empty then None
      else
         let char = List.head a
         let check = b |> List.filter (fun i -> List.contains char i)
         
         if check.Length = 2 then Some char
         else checkInAll (List.tail a) b 
   
   
   let day3p1 (backpacks: string list) =
      
      let rec inner score (remaining: string list) =
         if remaining = [] then
            printfn $"{score}"
            score
         else
            let backpack = List.head remaining
            let chars = backpack.ToCharArray() |> Array.toList
            let charsA, charsB = halveArray List.empty chars (chars.Length/2)
        
            let char =
               match checkInAll charsA [charsB] with
               | None -> 0
               | Some c ->
                  let asc = c |> int
                  let final =
                     if asc > 96 then (asc - 96)
                     else (asc - 38)
                  final
            inner (score + char) (List.tail remaining)
      inner 0 backpacks
      
   let day3p2 backpacks =
      
      let groups = splitByThrees backpacks
      
      let rec inner groups sum =
         if groups = [] then
            sum
         else
            let (group: string list) = List.head groups
            let groupChars = group |> List.map (fun i -> i.ToCharArray() |> Array.toList)
            let char =                                 
               match checkInAll (List.head groupChars) (List.tail groupChars) with   
               | None -> 0                             
               | Some c ->
                  let asc = c |> int                   
                  let final =                          
                     if asc > 96 then (asc - 96)       
                     else (asc - 38)
                  final                                
            inner (List.tail groups) (sum + char)
      inner groups 0       
      
      
   let day1 v =
      let filePath = "elf.txt"
      let rows = File.ReadLines filePath |> Seq.toList
      let finalRows = rows |> splitByBlankLine |> List.rev
      for row in finalRows do
         printfn $"{row}"
      let answer = findElf finalRows
      printfn $"{answer}"
   
   let day2 v =
      let filePath = "RPS.txt"
      let rows = File.ReadLines filePath |> Seq.toList
      //let answer = calcScore rows
      let answer = chooseMoves rows 
      printfn $"{answer}"
      
   let day3 v =
      let filePath = "backpacks.txt"
      let backpacks = File.ReadLines filePath |> Seq.toList
      //day3p1 backpacks
      let m = day3p2 backpacks
      printfn $"{m}"