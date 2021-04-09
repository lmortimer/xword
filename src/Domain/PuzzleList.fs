module Domain.PuzzleList

// parser for public/puzzles.json

open Fable.SimpleJson
open Fable.Core

let jsonStringToPuzzleList (input: string) =

    Json.tryParseAs<PuzzleList> input