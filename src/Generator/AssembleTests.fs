module Generator.AssembleTests

open NUnit.Framework

open Domain
open Grid
open Generator.Assemble

[<Test>]
let TestGenerateWordListWithSubstrings () =
    
    let emptyWordOutput = generateWordListWithPrefixStrings [""]
    Assert.IsEmpty(emptyWordOutput)
    
    let singleLetterOutput = generateWordListWithPrefixStrings ["A"]
    Assert.AreEqual(["A"], singleLetterOutput)
    
    let multiLetterOutput = generateWordListWithPrefixStrings ["bear"]    
    Assert.AreEqual(["b"; "be"; "bea"; "bear"], multiLetterOutput)

    let multipleWordsOutput = generateWordListWithPrefixStrings ["hi"; "yo"]
    Assert.AreEqual(["h"; "hi"; "y"; "yo"], multipleWordsOutput)
    
[<Test>]
let TestAssembleWordsWithExactMatch () =
            
    let wordList = ["aa"; "ab"; "ac"; "a"]

    let grid = generateGrid 2 wordList
        
    Assert.AreEqual("aa\nab", gridToAscii grid)
    

[<Test>]
let TestAssembleWordsHandlesPrefixesFromWordList () =
    
    let wordList = ["aa"; "ab"; "ac"]

    let grid = generateGrid 2 wordList
        
    Assert.AreEqual("aa\nab", gridToAscii grid)
    
[<Test>]
let TestAssembleAMini () =
    
    let wordList = ["choo"; "lawn"; "coins"; "duke"; "stud"; "cds"; "clout"; "haiku"; "owned"; "ons"]

    let grid = generateGrid 5 wordList
    
    printfn "%A" (gridToAscii grid)
        
//    Assert.AreEqual("aa\nab", gridToAscii grid)