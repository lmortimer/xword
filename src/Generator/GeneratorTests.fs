module Generator.GeneratorTests

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
    let singleLetterResults = findHorizontalLocationsForWord "A" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Across}], singleLetterResults)

    // but not two letters
    let twoLetterResult = findHorizontalLocationsForWord "AB" grid
    Assert.IsEmpty(twoLetterResult)
    
[<Test>]
let TestFindHorizontalLocationsForTwoByTwoEmptyGrid () =
    let grid = [ [ Cell.Black; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    // a single letter fits in each of the cells
    let singleLetterResults = findHorizontalLocationsForWord "A" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Across }; { RowIndex = 0; ColumnIndex = 1; Direction = Across }; { RowIndex = 1; ColumnIndex = 0; Direction = Across }; { RowIndex = 1; ColumnIndex = 1; Direction = Across }], singleLetterResults)

    // two letters fits on both rows
    let twoLetterResult = findHorizontalLocationsForWord "AB" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Across }; { RowIndex = 1; ColumnIndex = 0; Direction = Across }], twoLetterResult)
    
    // but not three letters
    let threeLetterResult = findHorizontalLocationsForWord "ABC" grid
    Assert.IsEmpty(threeLetterResult)
    
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
    let locationsForTwoLetterWord = findHorizontalLocationsForWord "AB" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Across }; { RowIndex = 1; ColumnIndex = 0; Direction = Across }], locationsForTwoLetterWord)
    
    // but not one that would match but is too large for the grid
    let locationsForWordTooLarge = findHorizontalLocationsForWord "ABCCCCC" grid
    Assert.IsEmpty(locationsForWordTooLarge)
    
    // but not one that doesn't match
    let locationsForWordThatShouldOnlyMatchSecondRow = findHorizontalLocationsForWord "CC" grid
    Assert.AreEqual([{ RowIndex = 1; ColumnIndex = 0; Direction = Across }], locationsForWordThatShouldOnlyMatchSecondRow)

    
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
    let locationsForThreeLetterWordThatFits = findHorizontalLocationsForWord "ABA" gridWithAGap
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Across }], locationsForThreeLetterWordThatFits)
    
    // but not one that doesn't match
    let locationsForWordTooLarge = findHorizontalLocationsForWord "ABCCCC" gridWithAGap
    Assert.IsEmpty(locationsForWordTooLarge)
    
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
    let wordFitsLocations = findVerticalLocationsForWord "A" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Down}], wordFitsLocations)

    // but not two letters
    let wordTooLargeLocations = findVerticalLocationsForWord "AB" grid
    Assert.IsEmpty(wordTooLargeLocations)
    
[<Test>]
let TestFindVerticalLocationsForTwoByTwoEmptyGrid () =
    let grid = [ [ Cell.Black; Cell.Black;]; [ Cell.Black; Cell.Black;]  ]
    
    // a single letter fits in each of the cells
    let singleLetterFitsInAllCellsLocations = findVerticalLocationsForWord "A" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Down }; { RowIndex = 1; ColumnIndex = 0; Direction = Down }; { RowIndex = 0; ColumnIndex = 1; Direction = Down }; { RowIndex = 1; ColumnIndex = 1; Direction = Down }], singleLetterFitsInAllCellsLocations)

    // two letters fits on both columns
    let wordFitsInBothColumnLocations = findVerticalLocationsForWord "AB" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Down }; { RowIndex = 0; ColumnIndex = 1; Direction = Down }], wordFitsInBothColumnLocations)
    
    // but not three letters
    let wordTooLargeToFitAnywhere = findVerticalLocationsForWord "ABC" grid
    Assert.IsEmpty(wordTooLargeToFitAnywhere)
    
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
    let sameLetterStartingFitsInBothColumnLocations = findVerticalLocationsForWord "AB" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Down }; { RowIndex = 0; ColumnIndex = 1; Direction = Down }], sameLetterStartingFitsInBothColumnLocations)
    
    // but not one that would match but is too large for the grid
    let sameLetterStartingButTooLarge = findVerticalLocationsForWord "ABCCCCC" grid
    Assert.IsEmpty(sameLetterStartingButTooLarge)
    
    // but not one that doesn't match
    let wordDoesNotMatchStartingLettersLocations = findVerticalLocationsForWord "CC" grid
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 1; Direction = Down }], wordDoesNotMatchStartingLettersLocations)

    
    // but not three letters even if it starts out matching
    let startingLettersMatchButWordIsTooLargeLocations = findHorizontalLocationsForWord "ABC" grid
    Assert.IsEmpty(startingLettersMatchButWordIsTooLargeLocations)
    
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
    let wordMatchesGapLocations = findVerticalLocationsForWord "ABA" gridWithAGap
    Assert.AreEqual([{ RowIndex = 0; ColumnIndex = 0; Direction = Down }; { RowIndex = 0; ColumnIndex = 1; Direction = Down }; { RowIndex = 0; ColumnIndex = 2; Direction = Down }], wordMatchesGapLocations)
    
    // but not one that doesn't match
    let wordStartsButDoesntMatchGapLocations = findVerticalLocationsForWord "ABCCCC" gridWithAGap
    Assert.IsEmpty(wordStartsButDoesntMatchGapLocations)
    
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
    
[<Test>]
let TestGenerateGrid () =
    
    let grid = makeEmptyGrid 3
    
    let firstHorizontalLocationForMan = findHorizontalLocationsForWord "man" grid |> List.head
    let gridWithMan = placeHorizontalWordOnGrid "man" firstHorizontalLocationForMan grid
        
    let expectedGridWithManOnFirstRow = [
        [
            Cell.White {
                Number = None
                Solution = "m"
                Guess = ""
                Solved = false
                Id = 0
            };
            Cell.White {
                Number = None
                Solution = "a"
                Guess = ""
                Solved = false
                Id = 0
            };
            Cell.White {
                Number = None
                Solution = "n"
                Guess = ""
                Solved = false
                Id = 0
            }
        ];
        [Cell.Black; Cell.Black; Cell.Black];
        [Cell.Black; Cell.Black; Cell.Black]]
    
    // m a n 
    // . . .
    // . . .
    Assert.AreEqual(expectedGridWithManOnFirstRow, gridWithMan)
            
    let firstVerticalLocationForMin = findVerticalLocationsForWord "man" gridWithMan |> List.head
    let gridWithManAndMin = placeVerticalWordOnGrid "min" firstVerticalLocationForMin gridWithMan
        
    let expectedGridWithManOnFirstRowAndMinOnFirstColumn = [
        [
            Cell.White {
                Number = None
                Solution = "m"
                Guess = ""
                Solved = false
                Id = 0
            };
            Cell.White {
                Number = None
                Solution = "a"
                Guess = ""
                Solved = false
                Id = 0
            };
            Cell.White {
                Number = None
                Solution = "n"
                Guess = ""
                Solved = false
                Id = 0
            }
        ];
        [
            Cell.White {
                Number = None
                Solution = "i"
                Guess = ""
                Solved = false
                Id = 0
            }; Cell.Black; Cell.Black
        ];
        [
            Cell.White {
                Number = None
                Solution = "n"
                Guess = ""
                Solved = false
                Id = 0
            }; Cell.Black; Cell.Black
        ]]
        
    // m a n
    // i . .
    // n . .
    Assert.AreEqual(expectedGridWithManOnFirstRowAndMinOnFirstColumn, gridWithManAndMin)
    
    let firstVerticalLocationForNo = findVerticalLocationsForWord "no" gridWithManAndMin |> List.head
    let gridWithManAndMinAndNo = placeVerticalWordOnGrid "no" firstVerticalLocationForNo gridWithManAndMin
        
    let expectedGridWithManOnFirstRowAndMinOnFirstColumnAndAnoOnSecondColumn = [
        [
            Cell.White {
                Number = None
                Solution = "m"
                Guess = ""
                Solved = false
                Id = 0
            };
            Cell.White {
                Number = None
                Solution = "a"
                Guess = ""
                Solved = false
                Id = 0
            };
            Cell.White {
                Number = None
                Solution = "n"
                Guess = ""
                Solved = false
                Id = 0
            }
        ];
        [
            Cell.White {
                Number = None
                Solution = "i"
                Guess = ""
                Solved = false
                Id = 0
            }
            Cell.White {
                Number = None
                Solution = "n"
                Guess = ""
                Solved = false
                Id = 0
            };
            Cell.Black
        ];
        [
            Cell.White {
                Number = None
                Solution = "n"
                Guess = ""
                Solved = false
                Id = 0
            }
            Cell.White {
                Number = None
                Solution = "o"
                Guess = ""
                Solved = false
                Id = 0
            }
            Cell.Black
        ]]
        
    // m a n
    // i n .
    // n o .
    Assert.AreEqual(expectedGridWithManOnFirstRowAndMinOnFirstColumnAndAnoOnSecondColumn, gridWithManAndMinAndNo)
    
    