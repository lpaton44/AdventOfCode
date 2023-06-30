namespace AdventOfCode

open System

module Days = 

   open System.IO
   
   //day 1 methods
   
   let findNextBlank lines =
      let index = List.tryFindIndex String.IsNullOrWhiteSpace lines
      match index with
               | Some i ->
                  if i = 0 then
                     None, List.tail lines
                  else
                     let line = List.takeWhile (fun i -> not (i = "")) lines
                     let remaining = List.removeManyAt 0 (i + 1) lines
                     Some line, remaining
               | None ->
                  let remaining = lines 
                  Some remaining, [] 
      
   let splitByBlankLine (data: string List) =
      let rec inner (lists: string list list) (rest: string List) =
         if rest = [] then lists
         else
            let addLine, remaining = findNextBlank rest 
            match addLine, remaining with
               | None, _ -> inner lists remaining
               | Some line, [] -> line::lists
               | Some line, _ -> inner (line::lists) remaining
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
   let playScore = ["Rock", 1; "Paper", 2; "Scissors", 3] |> Map.ofList
   let resultScore = ["Win", 6; "Draw", 3; "Lose", 0] |> Map.ofList
   let playConversions = ["A", "Rock"; "X", "Rock"; "B", "Paper"; "Y", "Paper"; "Z", "Scissors"; "C", "Scissors"]|> Map.ofList
   let outcomeConversions= ["X", "Lose"; "Y", "Draw"; "Z", "Win" ]|> Map.ofList
   let winningResults = ["Rock", "Paper"; "Paper", "Scissors"; "Scissors", "Rock"] |> Map.ofList
   let losingResults = ["Rock", "Scissors"; "Paper", "Rock"; "Scissors", "Paper"] |> Map.ofList
     
   let checkResult playerOne playerTwo =
      match playerOne, playerTwo with
      | x, y when (x = y) -> resultScore.["Draw"]
      | _, _ ->
         if winningResults[playerOne] = playerTwo then resultScore.["Win"]
         else resultScore.["Lose"]
   
   let calcScore rows =  
      let rec inner (rows: string list) score =
         if rows = [] then score
         else
            let row = List.head rows
            let plays = row.Split " " |> Array.toList |> List.map (fun i -> i.Trim())
            let roundScore =
                  let playerOne = playConversions[plays[0]]
                  let playerTwo = playConversions[plays[1]]
                  let playScore = playScore[playerTwo]
                  let resultScore = checkResult playerOne playerTwo 
                  playScore + resultScore
            inner (List.tail rows) (score + roundScore)     
      inner rows 0       
   
   let getMove playerOne outcome =
      let result =
         match playerOne, outcome with
         | _, "Win" -> playScore[winningResults[playerOne]] 
         | _, "Draw" -> playScore[playerOne] 
         | _, "Lose" -> playScore[losingResults[playerOne]]
      result     
   
   let chooseMoves rows =
      let rec inner (rows: string list) score =
         if rows = [] then score
         else
            let row = List.head rows
            let plays = row.Split " " |> Array.toList |> List.map (fun i -> i.Trim())
            printfn $"{plays[0]}"
            printfn $"{plays[1]}"
            let playerOne = playConversions[plays[0]]
            printfn $"{playerOne}"

            let outcome = outcomeConversions[plays[1]]
            let roundScore = getMove playerOne outcome          
            inner (List.tail rows) (score + roundScore + resultScore[outcome])
      
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
      let answer = findElf finalRows
      printfn $"{answer}"
   
   let day2 v =
      let filePath = "RPS.txt"
      let rows = File.ReadLines filePath |> Seq.toList
      let answer = calcScore rows
      //let answer = chooseMoves rows 
      printfn $"{answer}"
      
   let day3 v =
      let filePath = "backpacks.txt"
      let backpacks = File.ReadLines filePath |> Seq.toList
      //day3p1 backpacks
      let m = day3p2 backpacks
      printfn $"{m}"