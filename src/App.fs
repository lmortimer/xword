module App

open System
open Fable.Core
open Fable.Core.JS
open Feliz
open Feliz.Bulma
open Fetch

open Domain.Grid
open Domain.Puzzle

// The game state is only ever one of these games
type GameState =
    | Loading
    | Ready
    | Started
    | Ended

// Application messages
type Msg =
    | Loaded of (Grid * Clue list)
    | StartGame
    | CheckSolution
    | GuessUpdated of (White * string)

// Type alias so we don't need to type Msg -> Unit everywhere
type Dispatch = (Msg -> Unit)

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

// State (cell) map. DEVELOP. INTEGRATE 

let checkCellsAndUpdateStateIfSolved' (state: State) f: State = 
    let grid = 
        state.grid
        |> gridMap f

    { state with grid = grid }


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
        |> List.map (List.map (fun c ->
                match c with
                | Black -> c
                | White whiteCell ->
                    if whiteCell.Id = updatingCell.Id then
                        White {whiteCell with Guess = v}
                    else
                        White whiteCell
            )
        )
        
    let newGrid' =
        state.grid
        |> gridMap (fun cell -> if cell.Id = updatingCell.Id then {cell with Guess = v} else cell)

    { state with grid = newGrid'; }

// state change. yeah. nah.
let update' = function
    | Loaded (grid, clues) -> fun state -> ({ state with gameState = Ready; grid = grid; clues = clues })
    | CheckSolution -> checkCellsAndUpdateStateIfSolved >> checkGridAndUpdateStateIfSolved
    | GuessUpdated (cell, v) -> updateGuess cell v // todo: state to last arg
    | StartGame -> fun state -> ({ state with startTime = Some DateTime.Now; gameState = Started })
    
let update (state: State) (msg: Msg) = update' msg state

// View methods

let renderWhiteCell (cell: White) (dispatch: Dispatch) = 

    let solvedClass = 
        match cell.Solved with
        | true -> "has-background-primary-light"
        | false-> "has-background-danger-light"
    

    let number = 
        match cell.Number with
        | None -> Html.div [ prop.className "cell-number-height"]
        | Some n -> Bulma.tag [ 
            tag.isRounded
            prop.className "cell-number-height"
            prop.text n
          ]

    Html.div [
        prop.children [
            number
            Bulma.input.text [ 
              prop.maxLength 1
              prop.classes ["character-cell"; solvedClass]
              prop.onChange (fun (guess: string) -> dispatch (Msg.GuessUpdated(cell, guess.ToUpper())))
              prop.value cell.Guess
            ] 
        ]
    ]



let renderCell (dispatch: Dispatch) (cell: Cell) = 
    let contents = 
        match cell with
        | Black -> Html.div []
        | White x -> renderWhiteCell x dispatch

    let className =
        match cell with
        | Black -> "black-cell"
        | White _ -> "white-cell"

    Html.td [
        prop.className className
        prop.children [ contents ]
    ]


let renderGrid (grid: Grid) (dispatch: Dispatch) = 

    let rows =
        grid
        |> List.map (fun row -> 
            Html.tr (row |> List.map (renderCell dispatch))
        )

    Html.table [
        Html.tbody rows
    ]


let renderClues clues direction =
    let renderedClues = 
        clues
        |> List.filter (fun clue -> clue.Direction = direction)
        |> List.map (fun clue -> Html.li (sprintf "%d - %s" clue.Number clue.Clue))

    Html.ul renderedClues

[<ReactComponent>]
let CrosswordComponent() =


    let initialState: State = {
        grid = [[ Black ]]
        clues = []
        gameState = GameState.Loading
        startTime = None
        endTime = None
    }


    let (state, dispatch) = React.useReducer(update, initialState)
    

    let loadData() = async {

        let! response = fetch "puzzle/nyt-mini-1.json" [] |> Async.AwaitPromise
        let! data = response.text() |> Async.AwaitPromise
        let (grid, clues) = jsonStringToGridAndClues data
    
        dispatch(Loaded(grid, clues))

    }

    React.useEffect(loadData >> Async.StartImmediate, [| |])

    let startButtonOrClues =
        match state.gameState with
        | Ready -> Bulma.button.a [ prop.text "Start Game"
                                    prop.onClick (fun _ -> dispatch StartGame) ]
        | _ -> Html.div [
                    Html.div [
                        Bulma.button.a [
                            button.isLarge
                            color.hasBackgroundLight
                            prop.text "Check"
                            prop.onClick (fun _ -> dispatch CheckSolution)
                        ]
                    ]
                    Html.div [
                        Html.h3 [
                            text.hasTextWeightBold   
                            prop.text "Across"
                        ]
                        renderClues state.clues Across
                    ]
                    Html.div [
                        Html.h3 [
                            text.hasTextWeightBold   
                            prop.text "Down"
                        ]
                        renderClues state.clues Down
                    ]]
                
    let timeTakenToSolve =
        match state.gameState with
        | GameState.Ended -> Html.h1 [
            text.hasTextWeightBold
            prop.text (sprintf "You solved a puzzle in %.0f seconds" (state.endTime.Value - state.startTime.Value).TotalSeconds)
            ]
        | _ -> Html.h1 ""
        

    match state.gameState with
    | Loading -> Html.h1 "Loading"
    | _ -> Html.div [
        Bulma.columns [
            Bulma.column [
                column.is12
                prop.children [
                    timeTakenToSolve
                ]
            ]
        ]
        Bulma.columns [
            Bulma.column [
                column.is3
                prop.children [
                    renderGrid state.grid dispatch
                ]
            ]
            Bulma.column [
                column.is3
                prop.className "content"
                prop.children [
                    startButtonOrClues
                ]
            ]
        ]
    ]
    

    


[<ReactComponent>]
let HelloWorld() = Html.div [
    prop.className ""
    prop.children [
        Bulma.hero [
            color.isPrimary
            prop.children [Bulma.heroBody "#starcraft Mini"]
        ]

        CrosswordComponent()
    ]
]