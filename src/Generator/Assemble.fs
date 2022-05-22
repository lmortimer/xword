module Generator.Assemble

open Domain.Grid
open Generator.Generator
open Generator.Verifier

//  ["bear"] -> ["bear"; "bea"; "be"; "b"]
let generateWordListWithSubstrings (wordList: string list): string list =
    ["fo"]

let generateGrid (size: int) (wordList: string list) =
    
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
        
    List.fold tryToPlaceWordHorizontally (makeEmptyGrid size) wordList