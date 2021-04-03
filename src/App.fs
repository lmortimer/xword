module App

open Feliz
open Feliz.Bulma

open View.Grid

// View methods



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