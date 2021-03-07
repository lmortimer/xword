module Main

open Feliz
open Feliz.Bulma
open Browser.Dom
open Fable.Core.JsInterop



ReactDOM.render(
    App.HelloWorld(),
    document.getElementById "feliz-app"
)