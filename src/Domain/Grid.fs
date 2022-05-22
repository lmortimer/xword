module Domain.Grid

type White = {
    Number: int option
    Solution: string
    Guess: string
    Solved: bool
    Id: int // uniquely identify each cell, needed by React
}

// Cells in the crossword are either Black or White
//type CellOf<'a> = 
//    | Black
//    | White of 'a
//    
//type Cell = CellOf<White>
//    
//let cellMap : ('a -> 'b) -> CellOf<'a> -> CellOf<'b> =
//    let map f = function
//        | Black -> Black
//        | White w -> White (f w)
//    in map
    
type Cell =
    | Black
    | White of White
    
// If we perform an operation on a cell it's always "Do something to a White cell"    
let cellMap (f: White -> White) (cell: Cell): Cell =
    match cell with
    | Black -> Black
    | White w -> White (f w)
        

// And the Grid itself is stored as a list of list of Cells
type Grid = Cell list list

let gridMap (f: White -> White) (grid: Grid): Grid =    
    grid |> List.map (List.map (cellMap f))

let gridToArray (grid: Grid): Cell[][] =
    grid
    |> List.toArray
    |> Array.map List.toArray

let gridArrayToGrid(gridArray: Cell[][]): Grid =
    gridArray
    |> Array.toList
    |> List.map Array.toList
    
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


/// initialises a grid of height and width dimension filled with Black cells
let makeEmptyGrid (dimension: int): Grid = 

    [ for _ in 1 .. dimension -> [ for _ in 1 .. dimension -> Cell.Black ] ]
    
let gridToAscii (grid: Grid): string =
    grid
    |> List.map (fun row ->
        row
        |> List.map (fun cell ->
            match cell with
            | White w -> w.Solution
            | Black -> "."))
        |> List.map (fun stringsInRow -> String.concat "" stringsInRow)
    |> List.reduce (fun row acc -> row + "\n" + acc )

// The types for the clues are completely independent from the Grid
type Direction = Down | Across
type Clue = {
    Direction: Direction
    Number: int
    Clue: string
}
