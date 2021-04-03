module App

open Feliz
open Feliz.Bulma

open View.Crossword

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