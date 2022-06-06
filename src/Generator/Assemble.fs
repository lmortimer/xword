module Generator.Assemble

open Domain.Grid
open Generator.Generator
open Generator.Verifier

        
// when generating the grid we need to OK partially completed words        
//  ["bear"] -> ["b"; "be"; "bea"; "bear"]
let generateWordListWithPrefixStrings (wordList: string list): string list =
    
    let wordToPrefixes (word: string) =
        seq {
            for i in 0..( word.Length - 1) -> word.Substring(0, i + 1)
        } |> Seq.toList
    
    wordList
    |> List.map wordToPrefixes
    |> List.concat

let generateGrid (size: int) (wordList: string list) =
    
    // augment wordList with all prefix strings of each word
    // we validate against this interim wordList as we incrementally
    // build the grid
    let interimWordlist = generateWordListWithPrefixStrings wordList
    
    let tryToPlaceWordHorizontally (grid: Grid) (word: string) : Grid =
        
        // TODO rewrite this when not tired
        let placeWordOnGridAndReturnIfValid (word: string) (coord: Coord) (grid: Grid): Grid =
            let unverifiedGridWithWord = placeHorizontalWordOnGrid word coord grid
        
            match (verifyGrid unverifiedGridWithWord interimWordlist) with
            | true -> unverifiedGridWithWord
            | false -> grid
        
        let horizontalLocationsForWord = findHorizontalLocationsForWord word grid
          
        match horizontalLocationsForWord.Length with
        | 0 -> grid
        | _ -> placeWordOnGridAndReturnIfValid word horizontalLocationsForWord.Head grid
        
        
    let tryToPlaceWordVertically (grid: Grid) (word: string) : Grid =
        let invertedGrid = invertGrid grid
        
        tryToPlaceWordHorizontally invertedGrid word
        |> invertGrid
                
    List.fold tryToPlaceWordHorizontally (makeEmptyGrid size) interimWordlist
