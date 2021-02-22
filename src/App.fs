module App

open Feliz
open Elmish
open Fable.Core.JsInterop


// Domain types
type ClueDirection = Up | Down
type Instruction = int * ClueDirection
type Clue = Instruction * string

type White = {
    Solution: string
    Guess: string
    Solved: bool
    Id: int
}

type Cell = 
    | Black
    | White of White


type Grid = Cell list list


// React
type Msg = 
    | CheckSolution
    | GuessUpdated of (White * string)

type Dispatch = (Msg -> Unit)

// View methods

let renderWhiteCell (cell: White) (dispatch: Dispatch) = 

    let solvedClass = 
        match cell.Solved with
        | true -> "cell-correct"
        | false-> "cell-incorrect"

    Html.input [ 
      prop.maxLength 1
      prop.classes ["character-cell"; solvedClass]
      prop.onChange (fun (guess: string) -> dispatch (Msg.GuessUpdated(cell, guess)))
      prop.value cell.Guess
    ] 


let renderCell (dispatch: Dispatch) (cell: Cell) = 
    let contents = 
        match cell with
        | Black -> Html.h3 "x"
        | White x -> renderWhiteCell x dispatch

    Html.td contents


let renderGrid (grid: Grid) (dispatch: Dispatch) = 

    let renderCellWithDispatch = renderCell dispatch

    let rows =
        grid
        |> List.map (fun row -> 
            Html.tr (row |> List.map (renderCell dispatch))
        )

    Html.table [
        Html.tbody rows
    ]




type State = { grid: Grid }

let random = System.Random()

let makeCell solution = 
    White { Solution = solution; Guess = ""; Solved = false; Id = random.Next()}

let initialState: State = {
    grid = [
        [Black; Black; makeCell "T"; makeCell "V"; makeCell "S";]
        [makeCell "B"; makeCell "R"; makeCell "A"; makeCell "I"; makeCell "N";]
        [makeCell "D"; makeCell "U"; makeCell "N"; makeCell "N"; makeCell "O";]
        [makeCell "A"; makeCell "S"; makeCell "K"; makeCell "E"; makeCell "W";]
        [makeCell "Y"; makeCell "E"; makeCell "S"; Black; Black]
    ]
}

let checkWhiteCell (whiteCell: White): White =
    if whiteCell.Solution = whiteCell.Guess then { whiteCell with Solved = true } else { whiteCell with Solved = false }

let checkSolution (state: State): State = 
    let grid = 
        state.grid
        |> List.map (fun row -> 
            row |> List.map (fun cell -> 
                    match cell with
                    | Black -> cell
                    | White c -> Cell.White (checkWhiteCell c )
            )
        )

    { grid = grid }


let updateCheckSolution state = 
    Browser.Dom.console.log("Dispatched")
    let newState = checkSolution state
    Browser.Dom.console.log(newState)
    newState

let updateGuess state cell v =
    Browser.Dom.console.log("update guess called")
    Browser.Dom.console.log(state)
    Browser.Dom.console.log(cell)
    Browser.Dom.console.log(v)

    let newGrid = 
        state.grid
        |> List.map (fun row -> 
            row
            |> List.map (fun c ->
                match c with
                | Black -> c
                | White whiteCell -> if whiteCell.Id = cell.Id then Cell.White {whiteCell with Guess = v} else Cell.White whiteCell
            )
        )

    { grid = newGrid }


let update (state: State) = function
    | CheckSolution -> updateCheckSolution state
    | GuessUpdated (cell, v) -> updateGuess state cell v


let crosswordComponent = React.functionComponent(fun () ->
    let (state, dispatch) = React.useReducer(update, initialState)
    
    Html.div [
        Html.input [
            prop.value "Check"
            prop.type'.button
            prop.onClick (fun _ -> dispatch CheckSolution)
        ]
        Html.hr []
        renderGrid state.grid dispatch
    ]
)

[<ReactComponent>]
let HelloWorld() = Html.div [
    Html.h1 "#starcraft Mini"

    crosswordComponent()

]