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

// The types for the clues are completely independent from the Grid
type Direction = Down | Across
type Clue = {
    Direction: Direction
    Number: int
    Clue: string
}
