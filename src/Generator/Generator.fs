module Generator.Generator

open Domain.Grid

/// initialises a grid of height and width dimension filled with Black cells
let makeEmptyGrid (dimension: int): Grid = 

    [ for _ in 1 .. dimension -> [ for _ in 1 .. dimension -> Cell.Black ] ]
    
// index of the row, index of the column for the starting letter
type Coord = int * int * Direction
let findLocationsForWord (word: string) (grid: Grid): Result<Coord list, string> =
        
    let firstRow = grid |> List.head
    
    let firstCoordForRow (word: string) (row: Cell list) (rowIndex: int): Result<Coord list, string> =
        let windows =
            row
            |> List.indexed
            |> List.windowed word.Length
            |> List.map (fun window ->
                let windowStartingIndex = window |> List.head |> fst

                // does the word fit the window
                let letterCheckInWindow =
                    window
                    |> List.map (fun v ->
                        let cellIndex = fst v
                        let cell = snd v
                        
                        match cell with
                        | Black -> true
                        | White cell when cell.Solution = word.[cellIndex].ToString() -> true // else white and character matches is true
                        | _ -> false
                        )
                    
//                printfn $"Letter check in window: {letterCheckInWindow}"
                
                // if letterCheckInWindow is all true then the word matches
                if List.contains false letterCheckInWindow then Result.Error "Not found " else Result.Ok (rowIndex, windowStartingIndex, Across))
            
        printfn "Windows: %A" windows
        let successfulMatches =
            windows
            |> List.choose (fun result ->
                            match result with
                            | Ok x -> Some x
                            | Error _ -> None)

        if successfulMatches.IsEmpty then Result.Error "No matching windows" else Result.Ok successfulMatches
                

    firstCoordForRow word firstRow 0
    
    // look vertically for slices
    
    // see if word fits around what is there