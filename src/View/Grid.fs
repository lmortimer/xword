module View.Grid
// Feliz rendering functions for the Grid

open Domain.Grid
open Domain.Puzzle
open React.Grid

open Feliz
open Feliz.Bulma
open Fetch

open Fable.Core
open Fable.Core.JS

// Type alias so we don't need to type Msg -> Unit everywhere
type Dispatch = (Msg -> Unit)

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
    

    