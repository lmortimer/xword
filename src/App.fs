module App

open Feliz
open Feliz.Bulma
open Feliz.Router

open View

[<ReactComponent>]
let Router() =
    let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
    React.router [
        router.onUrlChanged updateUrl
        router.children [
            match currentUrl with
            | [ ] -> Page.Home.HomePage()
            | [ "play-puzzle"; Route.Query ["puzzle", puzzle]] -> Page.Game.GamePage(puzzle)
            | [ "users"; Route.Int userId ] -> Html.h1 (sprintf "User ID %d" userId)
            | otherwise -> Html.h1 "Not found"
        ]
    ]

[<ReactComponent>]
let MainComponent() = Html.div [
    prop.className ""
    prop.children [
        Bulma.hero [
            color.isPrimary
            prop.children [Bulma.heroBody "#starcraft Mini"]
        ]
    ]
]