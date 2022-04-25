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
let TestFindHorizontalLocationsForOneByOneEmptyGrid () =
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
let TestFindHorizontalLocationsForTwoByTwoEmptyGrid () =
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
let TestFindHorizontalLocationsForPartialWordRow () =
    let grid = [
        [ Cell.White { 
        Number = Some 1
        Solution = "A"
        Guess = ""; 
        Solved = false; 
        Id = 123
        }; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]

    // a two letter word that starts with the correct letter fits on both rows
    match (findHorizontalLocationsForWord "AB" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Across }; { RowIndex = 1; ColumnIndex = 0; Direction = Across }])
    | Error _ -> Assert.Fail("Should have been able to place two letters")
    
    // but not one that would match but is too large for the grid
    match (findHorizontalLocationsForWord "ABCCCCC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place large word")
    | Error _ -> Assert.Pass()
    
    // but not one that doesn't match
    // TODO should this match on the second row?
    match (findHorizontalLocationsForWord "CC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to match on the first row")
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
let TestInvertGrid () =
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
        
    let newGrid = invertGrid threeByThreeGrid
    
    Assert.AreEqual(expectedGrid, newGrid)
    
[<Test>]
let TestFindVerticalLocationsForOneByOneEmptyGrid () =
    let grid = [ [ Cell.Black;];  ]
    
    // a single letter fits
    match (findVerticalLocationsForWord "A" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Down}])
    | Error _ -> Assert.Fail("Should have been able to place single letter")

    // but not two letters
    match (findVerticalLocationsForWord "AB" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place two letters")
    | Error _ -> Assert.Pass()
    
[<Test>]
let TestFindVerticalLocationsForTwoByTwoEmptyGrid () =
    let grid = [ [ Cell.Black; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    // a single letter fits in each of the cells
    match (findVerticalLocationsForWord "A" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Down }; { RowIndex = 1; ColumnIndex = 0; Direction = Down }; { RowIndex = 0; ColumnIndex = 1; Direction = Down }; { RowIndex = 1; ColumnIndex = 1; Direction = Down }])
    | Error _ -> Assert.Fail("Should have been able to place single letter")

    // two letters fits on both columns
    match (findVerticalLocationsForWord "AB" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Down }; { RowIndex = 0; ColumnIndex = 1; Direction = Down }])
    | Error _ -> Assert.Fail("Should have been able to place two letters")
    
    // but not three letters
    match (findVerticalLocationsForWord "ABC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place three letters")
    | Error _ -> Assert.Pass()
    
[<Test>]
let TestFindVerticalLocationsForPartialWordRow () =
    let grid = [
        [ Cell.White { 
        Number = Some 1
        Solution = "A"
        Guess = ""; 
        Solved = false; 
        Id = 123
        }; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    // a two letter word that starts with the correct letter fits on both columns
    match (findVerticalLocationsForWord "AB" grid) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Down }; { RowIndex = 0; ColumnIndex = 1; Direction = Down }])
    | Error _ -> Assert.Fail("Should have been able to place two letters")
    
    // but not one that would match but is too large for the grid
    match (findVerticalLocationsForWord "ABCCCCC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place large word")
    | Error _ -> Assert.Pass()
    
    // but not one that doesn't match
    // TODO should this match the second column?
    match (findVerticalLocationsForWord "CC" grid) with
    | Ok _ -> Assert.Fail("Should not have been able to place in the first column")
    | Error _ -> Assert.Pass()
    
    // but not three letters even if it starts out matching
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
            Cell.Black;
        ];
        [ 
            Cell.Black;
            Cell.Black;
            Cell.Black;
        ];
        [ 
            Cell.White { 
                Number = Some 1
                Solution = "A"
                Guess = ""; 
                Solved = false; 
                Id = 123
                }; 
            Cell.Black;
            Cell.Black;
        ];]

    // a two letter word that starts with the correct letter fits
    match (findVerticalLocationsForWord "ABA" gridWithAGap) with
    | Ok x -> Assert.AreEqual(x, [{ RowIndex = 0; ColumnIndex = 0; Direction = Down }])
    | Error _ -> Assert.Fail("Should have been able to place two letters")
    
    // but not one that doesn't match
    match (findVerticalLocationsForWord "ABCCCC" gridWithAGap) with
    | Ok _ -> Assert.Fail("Should not have been able to place three letters")
    | Error _ -> Assert.Pass()
    
[<Test>]
let TestPlaceHorizontalWordOnGrid () =
    
    let grid = [ [ Cell.Black; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    let gridWithWordPlacedOnFirstRow = placeHorizontalWordOnGrid "AA" { RowIndex = 0; ColumnIndex = 0; Direction = Across } grid
    
    let expectedGridWithWordPlacedOnFirstRow = [
          [
           Cell.White { Number = None;
                        Solution = "A";
                        Guess = "";
                        Solved = false;
                        Id = 0 };
          Cell.White { Number = None;
                       Solution = "A";
                       Guess = "";
                       Solved = false;
                       Id = 0 }];
          [Black; Black]]
    
    Assert.AreEqual(expectedGridWithWordPlacedOnFirstRow, gridWithWordPlacedOnFirstRow)
    
    let gridWithWordPlacedOnSecondRowSecondColumn = placeHorizontalWordOnGrid "A" { RowIndex = 1; ColumnIndex = 1; Direction = Across } grid
    
    let expectedGridWithWordPlacedOnSecondRowSecondColumn = [
          [
           Cell.Black;
           Cell.Black
          ];
          [
           Cell.Black
           Cell.White { Number = None;
                       Solution = "A";
                       Guess = "";
                       Solved = false;
                       Id = 0 }
          ]]
    
    Assert.AreEqual(expectedGridWithWordPlacedOnSecondRowSecondColumn, gridWithWordPlacedOnSecondRowSecondColumn)
    
    
[<Test>]
let TestPlaceVerticalWordOnGrid () =
    
    let grid = [ [ Cell.Black; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    let gridWithWordPlacedOnFirstColumn = placeVerticalWordOnGrid "AA" { RowIndex = 0; ColumnIndex = 0; Direction = Down } grid
    
    let expectedGridWithWordPlacedOnFirstColumn = [
          [
              Cell.White { Number = None;
                           Solution = "A";
                           Guess = "";
                           Solved = false;
                           Id = 0 };
              Cell.Black;
          ];
          [
              Cell.White { Number = None;
                           Solution = "A"
                           Guess = "";
                           Solved = false;
                           Id = 0 };
              Cell.Black;
          ]]
    
    Assert.AreEqual(expectedGridWithWordPlacedOnFirstColumn, gridWithWordPlacedOnFirstColumn)
    
    let gridWithWordPlacedOnSecondRowSecondColumn = placeVerticalWordOnGrid "A" { RowIndex = 1; ColumnIndex = 1; Direction = Down } grid
    
    let expectedGridWithWordPlacedOnSecondRowSecondColumn = [
          [
           Cell.Black;
           Cell.Black
          ];
          [
           Cell.Black
           Cell.White { Number = None;
                       Solution = "A";
                       Guess = "";
                       Solved = false;
                       Id = 0 }
          ]]
    
    Assert.AreEqual(expectedGridWithWordPlacedOnSecondRowSecondColumn, gridWithWordPlacedOnSecondRowSecondColumn)