module View.Crossword

open Feliz
open Feliz.Bulma
open Fetch

open Fable.Core

open Domain.Grid
open Domain.Puzzle
open React.Grid
open View.Grid

[<ReactComponent>]
let CrosswordComponent(puzzleUrl: string) =

    let initialState: State = {
        grid = [[ Black ]]
        clues = []
        gameState = GameState.Loading
        startTime = None
        endTime = None
    }

    let (state, dispatch) = React.useReducer(update, initialState)    

    let loadData() = async {

        let! response = fetch puzzleUrl [] |> Async.AwaitPromise
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
        | _ -> Bulma.icon [ prop.className "fas fa-home" ]
        

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