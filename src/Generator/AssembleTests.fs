module Generator.AssembleTests

open NUnit.Framework

open Domain
open Grid
open Generator.Assemble

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