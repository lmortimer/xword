module Domain.PuzzleList

// parser for public/puzzles.json

open Fable.SimpleJson

// decodes public/puzzles.json
type PuzzleList = string list

let jsonStringToPuzzleList (input: string) =

    Json.tryParseAs<PuzzleList> input