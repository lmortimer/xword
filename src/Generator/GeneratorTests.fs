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
let TestFindHorizontalLocationForOneByOneEmptyGrid () =
    let grid = [ [ Cell.Black;];  ]
    
    // a single letter fits
    match (findHorizontalLocationsForWord "A" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Across}])
    | Error _ -> Assert.Fail("Should have been able to place single letter")

    // but not two letters
    match (findHorizontalLocationsForWord "AB" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place two letters")
    | Error _ -> Assert.Pass()
    
    
[<Test>]
let TestFindHorizontalLocationForTwoByTwoEmptyGrid () =
    let grid = [ [ Cell.Black; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    // a single letter fits in each of the cells
    match (findHorizontalLocationsForWord "A" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Across }; { RowIndex = 0; ColumnIndex = 1; Direction = Across }; { RowIndex = 1; ColumnIndex = 0; Direction = Across }; { RowIndex = 1; ColumnIndex = 1; Direction = Across }])
    | Error _ -> Assert.Fail("Should have been able to place single letter")

    // two letters fits on both rows
    match (findHorizontalLocationsForWord "AB" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Across }; { RowIndex = 1; ColumnIndex = 0; Direction = Across }])
    | Error _ -> Assert.Fail("Should have been able to place two letters")
    
    // but not three letters
    match (findHorizontalLocationsForWord "ABC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place three letters")
    | Error _ -> Assert.Pass()
    
[<Test>]
let TestFindHorizontalLocationForPartialWordRow () =
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
    //    | Ok x -> Assert.AreEqual(x, { RowIndex = 0; ColumnIndex = 0; Direction = Across })
    //    | Error _ -> Assert.Fail("Should have been able to place single letter")

    // a two letter word that starts with the correct letter fits on both rows
    match (findHorizontalLocationsForWord "AB" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Across }; { RowIndex = 1; ColumnIndex = 0; Direction = Across }])
    | Error _ -> Assert.Fail("Should have been able to place two letters")
    
    // but not one that would match but is too large for the grid
    match (findHorizontalLocationsForWord "ABCCCCC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place large word")
    | Error _ -> Assert.Pass()
    
    // but not one that doesn't match
    match (findHorizontalLocationsForWord "CC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place three letters")
    | Error _ -> Assert.Pass()
    
    // but not three letters
    match (findHorizontalLocationsForWord "ABC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place three letters")
    | Error _ -> Assert.Pass()
    
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
    match (findHorizontalLocationsForWord "ABA" gridWithAGap) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Across }])
    | Error _ -> Assert.Fail("Should have been able to place two letters")
    
    // but not one that doesn't match
    match (findHorizontalLocationsForWord "ABCCCC" gridWithAGap) with
    | Ok _ -> Assert.Fail("Should not have been able to place three letters")
    | Error _ -> Assert.Pass()
    
[<Test>]
let TestTranslateGridColumnsIntoRowRepresentation () =
    let threeByThreeGrid = [
        [ 
            Cell.White { 
                Number = Some 1
                Solution = "M"
                Guess = ""; 
                Solved = false; 
                Id = 123
            }; 
            Cell.White { 
                Number = None
                Solution = "A"
                Guess = ""; 
                Solved = false; 
                Id = 124
            };
            Cell.White { 
                Number = Some 1
                Solution = "N"
                Guess = ""; 
                Solved = false; 
                Id = 125
            }; 
        ];
        [ 
            Cell.White { 
                Number = None
                Solution = "I"
                Guess = ""; 
                Solved = false; 
                Id = 126
            }; 
            Cell.Black;
            Cell.White { 
                Number = None
                Solution = "O"
                Guess = ""; 
                Solved = false; 
                Id = 127
            }; 
        ];
        [ 
            Cell.White { 
                Number = None
                Solution = "N"
                Guess = ""; 
                Solved = false; 
                Id = 128
            }; 
            Cell.Black
            Cell.Black
        ]
    ]
    
    let expectedGrid = [
        [ 
            Cell.White { 
                Number = Some 1
                Solution = "M"
                Guess = ""; 
                Solved = false; 
                Id = 123
            };
            Cell.White { 
                Number = None
                Solution = "I"
                Guess = ""; 
                Solved = false; 
                Id = 126
            }
            Cell.White { 
                Number = None
                Solution = "N"
                Guess = ""; 
                Solved = false; 
                Id = 128
            }; 

        ];
        [ 
            Cell.White { 
                Number = None
                Solution = "A"
                Guess = ""; 
                Solved = false; 
                Id = 124
            };
            Cell.Black;
            Cell.Black;
        ];
        [ 
            Cell.White { 
                Number = Some 1
                Solution = "N"
                Guess = ""; 
                Solved = false; 
                Id = 125
            }
            Cell.White { 
                Number = None
                Solution = "O"
                Guess = ""; 
                Solved = false; 
                Id = 127
            }
            Cell.Black;
        ]
    ]
        
    let newGrid = translateGridColumnsIntoRowRepresentation threeByThreeGrid
    
    Assert.AreEqual(expectedGrid, newGrid)