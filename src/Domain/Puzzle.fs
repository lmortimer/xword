module Domain.Puzzle

// Translates Puzzles (definitions in JSON) into Grids which can be used in the game

open Fable.SimpleJson
open Domain.Grid

type private WhitePuzzleCell = {
    Number: int option
    Solution: string
}

type private PuzzleCell = 
    | Black
    | White of WhitePuzzleCell

type private Puzzle = {
    Grid: PuzzleCell list list
    Clues: Clue list
}

let private whitePuzzleCellToWhiteCell (puzzlecell: WhitePuzzleCell) (random: System.Random) =
    Cell.White { 
        Number = puzzlecell.Number 
        Solution = puzzlecell.Solution; 
        Guess = ""; 
        Solved = false; 
        Id = random.Next()
    }

let private puzzleToGrid (puzzle: Puzzle): Grid = 

    let random = System.Random()

    puzzle.Grid
    |> List.map (fun row -> 
        row
        |> List.map (fun cell ->
            match cell with
            | Black -> Cell.Black
            | White c -> (whitePuzzleCellToWhiteCell c random)
        )
    )

let jsonStringToGridAndClues (input: string): (Grid * Clue list) =
    let puzzle = input |> Json.parseAs<Puzzle>

    let grid = puzzleToGrid puzzle

    (grid, puzzle.Clues)