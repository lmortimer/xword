module View.CrosswordList

open Feliz
open Fetch
open Fable.Core
open Fable.SimpleJson

// decodes public/puzzles.json
type private PuzzleList = string list

type State = {
    crosswords: string list
}

type Msg = 
    | CrosswordListLoaded of string list

let update (state: State) = function
    | CrosswordListLoaded (crosswords) -> { state with crosswords = crosswords }

[<ReactComponent>]
let CrosswordList() =

    let (state, dispatch) = React.useReducer(update, { crosswords = [] })

    let loadData() = async {

        let! response = fetch "puzzles.json" [] |> Async.AwaitPromise
        let! data = response.text() |> Async.AwaitPromise

        match Json.tryParseAs<PuzzleList> data with
        | Ok v -> dispatch(CrosswordListLoaded(v))
        | Error err -> JS.console.error(err)
    }

    let crosswords = 
        state.crosswords 
        |> List.map (fun v -> Html.li v)

    React.useEffect(loadData >> Async.StartImmediate, [| |])

    Html.div [
        Html.h1 "Crosswords"
        Html.ul crosswords
    ]