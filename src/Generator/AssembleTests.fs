module Generator.AssembleTests

open NUnit.Framework

open Domain
open Grid
open Generator.Generator
open Generator.Verifier

[<Test>]
let TestAssemble () =
            
    let wordList = ["aa"; "ab"; "ac"; "a"]
    let emptyGrid = makeEmptyGrid 2
    
    let tryToPlaceWordHorizontally (grid: Grid) (word: string) : Grid =
        
        let gridWithWord =
            findHorizontalLocationsForWord word grid
//            |> Result.mapError failwith
            |> Result.map (fun locations ->
                let coord = List.head locations
                placeHorizontalWordOnGrid word coord grid)
            |> Result.bind (fun g ->
                match (verifyGrid g wordList) with
                | true -> Ok g
                | false -> Result.Error (sprintf "Error with grid: %A" g))
        
        match gridWithWord with
        | Ok g -> g
        | Error _ -> grid
    
    let firstRound = tryToPlaceWordHorizontally emptyGrid (wordList |> List.head) 
    
    let mutable grid = makeEmptyGrid 2
    
    for word in wordList do
        grid <- tryToPlaceWordHorizontally grid word
    
    printfn "%s" (gridToAscii grid)