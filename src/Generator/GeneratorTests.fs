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
let TestFindLocationForOneByOneEmptyGrid () =
    let grid = [ [ Cell.Black;];  ]
    
    // a single letter fits
    match (findLocationsForWord "A" grid) with
    | Ok x -> Assert.AreEqual(x, (0, 0, Across))
    | Error x -> Assert.Fail("Should have been able to place single letter")

    // but not two letters
    match (findLocationsForWord "AB" grid) with
    | Ok x -> Assert.Fail("Should not have been able to place two letters")
    | Error x -> Assert.Pass()
    
    
[<Test>]
let TestFindLocationForTwoByTwoEmptyGrid () =
    let grid = [ [ Cell.Black; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    // a single letter fits
    match (findLocationsForWord "A" grid) with
    | Ok x -> Assert.AreEqual(x, (0, 0, Across))
    | Error x -> Assert.Fail("Should have been able to place single letter")

    // two letters fit
    match (findLocationsForWord "AB" grid) with
    | Ok x -> Assert.AreEqual(x, (0, 0, Across))
    | Error x -> Assert.Fail("Should have been able to place two letters")
    
    // but not three letters
    match (findLocationsForWord "ABC" grid) with
    | Ok x -> Assert.Fail("Should not have been able to place three letters")
    | Error x -> Assert.Pass()
    
[<Test>]
let TestFindLocationForPartialWordRow () =
    let grid = [
        [ Cell.White { 
        Number = Some 1
        Solution = "A"
        Guess = ""; 
        Solved = false; 
        Id = 123
        }; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    // a single letter fits
    //    match (findLocationsForWord "A" grid) with
    //    | Ok x -> Assert.AreEqual(x, (0, 0, Across))
    //    | Error x -> Assert.Fail("Should have been able to place single letter")

    // a two letter word that starts with the correct letter fits
    match (findLocationsForWord "AB" grid) with
    | Ok x -> Assert.AreEqual(x, (0, 0, Across))
    | Error x -> Assert.Fail("Should have been able to place two letters")
    
    // but not one that would match but is too large for the grid
    match (findLocationsForWord "ABCCCCC" grid) with
    | Ok x -> Assert.Fail("Should not have been able to place large word")
    | Error x -> Assert.Pass()
    
    // but not one that doesn't match
    match (findLocationsForWord "CC" grid) with
    | Ok x -> Assert.Fail("Should not have been able to place three letters")
    | Error x -> Assert.Pass()
    
    // but not three letters
    match (findLocationsForWord "ABC" grid) with
    | Ok x -> Assert.Fail("Should not have been able to place three letters")
    | Error x -> Assert.Pass()
    
    let gridWithAGap = [
        [ 
            Cell.White { 
                Number = Some 1
                Solution = "A"
                Guess = ""; 
                Solved = false; 
                Id = 123
            }; 
            Cell.Black;
            Cell.White { 
                    Number = Some 1
                    Solution = "A"
                    Guess = ""; 
                    Solved = false; 
                    Id = 123
                    };
        ]]
        
    // a two letter word that starts with the correct letter fits
    match (findLocationsForWord "ABA" gridWithAGap) with
    | Ok x -> Assert.AreEqual(x, (0, 0, Across))
    | Error x -> Assert.Fail("Should have been able to place two letters")
    
    // but not one that doesn't match
    match (findLocationsForWord "ABCCCC" gridWithAGap) with
    | Ok x -> Assert.Fail("Should not have been able to place three letters")
    | Error x -> Assert.Pass()