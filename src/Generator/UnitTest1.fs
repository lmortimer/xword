module Generator.Tests

open NUnit.Framework
open Generator.Generator

open Domain.Grid


[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestMakeEmptyGrid () =
   
    Assert.AreEqual((makeEmptyGrid 1), [ [ Cell.Black ] ])
    Assert.AreEqual((makeEmptyGrid 2), [ [ Cell.Black; Cell.Black ]
                                         [ Cell.Black; Cell.Black ]; ])
    Assert.AreEqual((makeEmptyGrid 3), [ [ Cell.Black; Cell.Black; Cell.Black ];
                                         [ Cell.Black; Cell.Black ; Cell.Black ]
                                         [ Cell.Black; Cell.Black ; Cell.Black ]])

[<Test>]
let TestFindLocationForOneByOneGrid () =
    let grid = [ [ Cell.Black;];  ]
    
    // a single letter fits
    match (findLocationsForWord "A" grid) with
    | Ok x -> Assert.AreEqual(x, (0, 0, Down))
    | Error x -> Assert.Fail("Should have been able to place single letter")

    // but not two letters
    match (findLocationsForWord "AB" grid) with
    | Ok x -> Assert.Fail("Should not have been able to place two letters")
    | Error x -> Assert.Pass()