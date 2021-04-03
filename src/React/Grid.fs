module React.Grid

open System
open Domain.Grid

type GameState =
    | Loading
    | Ready
    | Started
    | Ended

type Msg =
    | Loaded of (Grid * Clue list)
    | StartGame
    | CheckSolution
    | GuessUpdated of (White * string)

// Application state managed by React
// startTime and endTime when the game starts (GameState Ready -> Started) and ends (Started -> Ended)
// these could be more purely modelled but the cost seems to outweigh the benefit
type State = {
    grid: Grid
    clues: Clue list
    gameState: GameState
    startTime: DateTime option
    endTime: DateTime option
}

let checkCellsAndUpdateStateIfSolved (state: State): State = 
    let grid = 
        state.grid
        |> gridMap (fun c -> { c with Solved = c.Solution = c.Guess })

    { state with grid = grid }

let checkGridAndUpdateStateIfSolved (state: State): State =
    let gridIsSolved  = 
        state.grid
        |> List.concat
        |> List.filter (fun c ->
            match c with
            | White whiteCell when whiteCell.Solved = false -> true
            | _ -> false
            )
        |> List.isEmpty

    match gridIsSolved with
    | true ->  { state with gameState = Ended; endTime = Some DateTime.Now }
    | false -> state
    
// Effectively called whenever a character is typed into a white square. Adds the character to the cell state
let updateGuess  updatingCell v state =

    let newGrid =
        state.grid
        |> gridMap (fun cell -> if cell.Id = updatingCell.Id then {cell with Guess = v} else cell)

    { state with grid = newGrid }

let update (state: State) = function
    | Loaded (grid, clues) -> ({state with gameState = Ready; grid = grid; clues = clues})
    | CheckSolution -> state |> checkCellsAndUpdateStateIfSolved |> checkGridAndUpdateStateIfSolved
    | GuessUpdated (cell, v) -> (updateGuess cell v state)
    | StartGame -> ({ state with startTime = Some DateTime.Now; gameState = Started })