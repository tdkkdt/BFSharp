namespace Tests

open CompilerLib
open NUnit.Framework
open System.Diagnostics

[<SyntaxCheckTests>]
type SyntaxCheckTests() =

    [<Test>]
    member this.OkTest() =
        Assert.AreEqual(SyntaxCheckResult.OK, SyntaxChecker.checkSyntax [Token.LoopStart; Token.LoopEnd; Token.IncrementData])

    [<Test>]
    member this.EmptyTest() =
        Assert.AreEqual(SyntaxCheckResult.CodeEmpty, SyntaxChecker.checkSyntax [])

    [<Test>]
    member this.LoopTest() =
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker.checkSyntax [Token.LoopStart])
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker.checkSyntax [Token.LoopEnd])
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker.checkSyntax [Token.LoopEnd; Token.LoopStart])
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker.checkSyntax [Token.LoopStart; Token.LoopEnd; Token.LoopStart])
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker.checkSyntax [Token.LoopStart; Token.IncrementData; Token.LoopEnd; Token.IncrementData; Token.LoopStart; Token.IncrementData])