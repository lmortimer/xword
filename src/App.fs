module App

open System
open Fable.Core
open Fable.Core.JS
open Feliz
open Feliz.Bulma

// Domain types
type White = {
    Number: int option
    Solution: string
    Guess: string
    Solved: bool
    Id: int
}

type Cell = 
    | Black
    | White of White


type Grid = Cell list list

// The clues and grid are completely independent
type Direction = Down | Across
type Clue = {
    Direction: Direction
    Number: int
    Clue: string
}


// 
type GameState =
    | Ready
    | Started
    | Ended

// React useReducer
type Msg =
    | StartGame
    | CheckSolution
    | GuessUpdated of (White * string)

type Dispatch = (Msg -> Unit)


type State = {
    grid: Grid
    gameState: GameState
    startTime: DateTime option
    endTime: DateTime option
}

let random = System.Random()


// Utility method to create the grid
let makeCell solution = 
    White { Number = None; Solution = solution; Guess = ""; Solved = false; Id = random.Next()}

let makeCellWithNumber solution number = 
    White { Number = (Some number); Solution = solution; Guess = ""; Solved = false; Id = random.Next()}

let initialState: State = {
    grid = [
        [Black; Black; makeCellWithNumber "T" 1; makeCellWithNumber "V" 2; makeCellWithNumber "S" 3;]
        [makeCellWithNumber "B" 4; makeCellWithNumber "R" 5; makeCell "A"; makeCell "I"; makeCell "N";]
        [makeCellWithNumber "D" 6; makeCell "U"; makeCell "N"; makeCell "N"; makeCell "O";]
        [makeCellWithNumber "A" 7; makeCell "S"; makeCell "K"; makeCell "E"; makeCell "W";]
        [makeCellWithNumber "Y" 8; makeCell "E"; makeCell "S"; Black; Black]
    ];
    gameState = GameState.Ready
    startTime = None
    endTime = None
}

let clues = [
    { Direction = Across; Number = 1; Clue = "Waiting room distractions"};
    { Direction = Across; Number = 4; Clue = "It makes up 2% of the body's weight, but uses 20% of it's energy"};
    { Direction = Across; Number = 6; Clue = "\"Beat's me!\""};
    { Direction = Across; Number = 7; Clue = "Slightly off-centre"};
    { Direction = Across; Number = 8; Clue = "Part of Y/N"};
    { Direction = Down; Number = 1; Clue = "Loses intentionally"};
    { Direction = Down; Number = 2; Clue = "Tree-climbing plant"};
    { Direction = Down; Number = 3; Clue = "What Syracuse NY once jokingly attempted to outlaw, after the harsh 1991-92 winter season"};
    { Direction = Down; Number = 4; Clue = "Time for cake and candles, for short"};
    { Direction = Down; Number = 5; Clue = "Misleading ploy"};
]

let isSolved (grid: Grid): bool =
    grid
    |> List.concat
    |> List.filter (fun c ->
        match c with
        | White whiteCell when whiteCell.Solved = false -> true
        | _ -> false
        )
    |> List.isEmpty
        
// Called when the 'Check' button is clicked. Currently does two things, should I split them out?
let checkSolution (state: State): State = 
    let grid = 
        state.grid
        |> List.map (fun row -> 
            row |> List.map (fun cell -> 
                    match cell with
                    | Black -> cell
                    | White c -> Cell.White { c with Solved = c.Solution = c.Guess }
            )
        )

    match isSolved grid with
    | true ->  { state with grid = grid; gameState = Ended; endTime = Some DateTime.Now }
    | false -> { state with grid = grid; }
    
// Effectively called whenever a character is typed into a white square. Adds the character to the cell state
let updateGuess state cell v =

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

    { state with grid = newGrid; }


let update (state: State) = function
    | CheckSolution -> checkSolution state
    | GuessUpdated (cell, v) -> updateGuess state cell v
    | StartGame -> { state with startTime = Some DateTime.Now; gameState = Started }

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

let crosswordComponent = React.functionComponent(fun () ->
    let (state, dispatch) = React.useReducer(update, initialState)
    
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
                        renderClues clues Across
                    ]
                    Html.div [
                        Html.h3 [
                            text.hasTextWeightBold   
                            prop.text "Down"
                        ]
                        renderClues clues Down
                    ]]
                
    let timeTakenToSolve =
        match state.gameState with
        | GameState.Ended -> Html.h1 [
            text.hasTextWeightBold
            prop.text (sprintf "You solved a puzzle in %.0f seconds" (state.endTime.Value - state.startTime.Value).TotalSeconds)
            ]
        | _ -> Html.h1 ""
        
    Html.div [
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
)

[<ReactComponent>]
let HelloWorld() = Html.div [
    prop.className ""
    prop.children [
        Bulma.hero [
            color.isPrimary
            prop.children [Bulma.heroBody "#starcraft Mini"]
        ]

        crosswordComponent()
    ]
]