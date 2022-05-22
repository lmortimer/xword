module Generator.VerifierTests

open NUnit.Framework

open Domain.Grid
open Generator.Verifier

[<Test>]
let TestEmptyVerify() =
    let emptyGrid = makeEmptyGrid 3
    let verifyEmptyGrid = verifyGrid emptyGrid [""]
    
    Assert.IsTrue(verifyEmptyGrid)
    
[<Test>]
let TestTwoByTwoGrid () =
                    
    // m a
    // i n
    let gridWithMaOnFirstRowInOnSecondRow = [
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
        ]]
    
    let verifyGridWithWordsMissingFromWordList = verifyGrid gridWithMaOnFirstRowInOnSecondRow ["ma"; "in"; "hello"; "world"]
    
    Assert.IsFalse(verifyGridWithWordsMissingFromWordList)
    
    let verifyGridWithWordsInWordList = verifyGrid gridWithMaOnFirstRowInOnSecondRow ["ma"; "in"; "mi"; "an"] 

    Assert.IsTrue(verifyGridWithWordsInWordList)

[<Test>]
let RegressionGridCase () =
    
    let grid = [
          [
           White {
               Number = None
               Solution = "a"
               Guess = ""
               Solved = false
               Id = 0
           }
           White {
                Number = None
                Solution = "a"
                Guess = ""
                Solved = false
                Id = 0
           }]
          [Black; Black]]

    let wordList = ["aa"; "ab"; "ac"; "a"]

    let verifyGridWithWordsInWordList = verifyGrid grid wordList 

    Assert.IsTrue(verifyGridWithWordsInWordList)