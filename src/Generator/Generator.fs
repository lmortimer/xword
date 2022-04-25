module Generator.Generator

open Domain.Grid

/// initialises a grid of height and width dimension filled with Black cells
let makeEmptyGrid (dimension: int): Grid = 

    [ for _ in 1 .. dimension -> [ for _ in 1 .. dimension -> Cell.Black ] ]
    
// index of the row, index of the column for the starting letter
type Coord = {
    RowIndex: int
    ColumnIndex: int
    Direction: Direction
}

/// So that we can apply the same findHorizontalLocationsForWord algorithm when searching for verticals 
///
/// Turns    M A N    Into    M I N 
///          I . O            A . .
///          N . .            N O .
/// 
let invertGrid (grid: Grid): Grid =
    
    // sanity check that the first row has as many cols as there are rows in the grid
    if grid |> List.head |> List.length <> grid.Length then failwith "Grid must be square"
    
    let gridAsArray = gridToArray grid
    
    //TODO need to flip directions and clues?

    // make the new grid by flipping columns and rows from the original grid
    List.init grid.Length (fun rowIndex ->
        List.init grid.Length (fun colIndex -> gridAsArray.[colIndex].[rowIndex]))
    
let findHorizontalLocationsForWord (word: string) (grid: Grid): Result<Coord list, string> =
            
    let allCoordsForRow (word: string) (rowIndex: int) (row: Cell list): Result<Coord list, string> =
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

                // if letterCheckInWindow is all true then the word matches
                if List.contains false letterCheckInWindow then Result.Error "Not found " else Result.Ok { RowIndex = rowIndex; ColumnIndex = windowStartingIndex; Direction = Across})
            
        let successfulMatches =
            windows
            |> List.choose (fun result ->
                            match result with
                            | Ok x -> Some x
                            | Error _ -> None)

        if successfulMatches.IsEmpty then Result.Error "No matching windows" else Result.Ok successfulMatches
                
    let allCoordsAcrossAllRows = 
        grid
        |> List.mapi (fun idx row -> allCoordsForRow word idx row)
        |> List.choose (fun result ->
                            match result with
                            | Ok x -> Some x
                            | Error _ -> None)
        |> List.concat
            
    if allCoordsAcrossAllRows.IsEmpty then Result.Error "No matching windows" else Result.Ok allCoordsAcrossAllRows
    
let findVerticalLocationsForWord (word: string) (grid: Grid): Result<Coord list, string> =
    
    // we translate the grid so we can apply the same findHorizontalLocationsForWord algorithm
    let locations =
        grid
        |> invertGrid
        |> findHorizontalLocationsForWord word
        
    // then just invert the coords and direction. magic!
    locations
    |> Result.map (fun coords ->
        coords
        |> List.map (fun coord -> { coord with RowIndex = coord.ColumnIndex; ColumnIndex = coord.RowIndex; Direction = Down }))

// assumes the word can be placed there, validation being done in the find*LocationsForWord methods
let placeHorizontalWordOnGrid (word: string) (coord: Coord) (grid: Grid): Grid =
    
    let gridAsArray = gridToArray grid
    
    for wordIndex in 0 .. word.Length - 1 do
        // TODO fix cell, number
        let newCell = Cell.White { Solution = word.[wordIndex].ToString(); Number = None; Guess = ""; Solved = false; Id = 0 }
        
        gridAsArray.[coord.RowIndex].[coord.ColumnIndex + wordIndex] <- newCell
        
    gridArrayToGrid gridAsArray
    
let placeVerticalWordOnGrid (word: string) (coord: Coord) (grid: Grid): Grid =
    
    grid
    |> invertGrid
    |> placeHorizontalWordOnGrid word coord
    |> invertGrid
