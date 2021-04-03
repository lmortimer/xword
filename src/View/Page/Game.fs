module View.Page.Game

open Feliz
open Fable.Core
open View.Crossword

[<ReactComponent>]
let GamePage(puzzleUrl: string) =
    CrosswordComponent(puzzleUrl)