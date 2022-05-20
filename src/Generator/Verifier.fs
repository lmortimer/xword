module Generator.Verifier

open Domain.Grid

// given a grid and a wordlist does it only contain valid words?

let verifyGrid (grid: Grid) (wordList: string list): bool =
    
    let extractWordsFromGridInHorizontalDirection (grid: Grid) (wordList: string list) = 
        let placeHolderForBlackCell = "~"
        
        // extract the words from a grid
        grid
        |> List.map (fun row ->
            row
            |> List.map (fun cell ->
                match cell with
                | White w -> w.Solution
                | Black -> placeHolderForBlackCell
            ))
        |> List.map (fun stringsInRow -> String.concat "" stringsInRow)
        |> List.map (fun stringifiedRow -> stringifiedRow.Split(placeHolderForBlackCell))
        |> List.map Array.toList
        |> List.concat
                
    let horizontalWords = extractWordsFromGridInHorizontalDirection grid wordList
    let verticalWords = extractWordsFromGridInHorizontalDirection (invertGrid grid) wordList
    
    let allWords = horizontalWords @ verticalWords
    
    // ensure all words are in the wordlist
    let allWordsThatArentInTheWordList = 
        allWords
        |> List.map (fun word -> List.contains word wordList)
        |> List.filter (fun v -> v = false)

    allWordsThatArentInTheWordList |> List.isEmpty
