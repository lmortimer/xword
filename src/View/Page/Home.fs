module View.Page.Home

open Feliz
open View.CrosswordList

[<ReactComponent>]
let HomePage() = 
    Html.div [
        prop.children [
            Html.h1 "Home"
            CrosswordList()
        ]
    ]