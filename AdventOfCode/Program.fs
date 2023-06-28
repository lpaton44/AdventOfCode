open AdventOfCode
open System 

let runP p v =
    let programme = p |> int
    match programme with 
    | 1 -> Days.day1 v
    | 2 -> Days.day2 v
    | 3 -> Days.day3 v 
    | _ -> printfn "Invalid number." 


[<EntryPoint>]
let main argv = 
        
    printf "Enter the problem you would like solved:
    1: Elf calorie counting  
    2: Rock Paper Scissors \n" 

    (*let p = Console.ReadLine().Trim()
    let programme = p |> int
    let s =     
        match programme with 
            | 1 -> "Enter file name: "
            | _ -> "Invalid Number."
    
    printf $"{s}" 
    let v = 
        if not (s = "") then Console.ReadLine().Trim()
        else "d"*)
    runP 3 "backpacks.txt"
    0