module Generator.Tests

open NUnit.Framework
open Generator.Generator


[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    let res = generatePuzzle 2

    Assert.AreEqual(res, 1)
