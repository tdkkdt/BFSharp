namespace Tests

open CompilerLib
open NUnit.Framework
open System.Diagnostics

[<SyntaxCheckTests>]
type SyntaxCheckTests() =

    [<Test>]
    member this.OkTest() =
        Assert.AreEqual(SyntaxCheckResult.OK, SyntaxChecker().CheckSyntax [Token.LoopStart; Token.LoopEnd; Token.IncrementData])

    [<Test>]
    member this.EmptyTest() =
        Assert.AreEqual(SyntaxCheckResult.CodeEmpty, SyntaxChecker().CheckSyntax [])

    [<Test>]
    member this.LoopTest() =
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker().CheckSyntax [Token.LoopStart])
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker().CheckSyntax [Token.LoopEnd])
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker().CheckSyntax [Token.LoopEnd; Token.LoopStart])
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker().CheckSyntax [Token.LoopStart; Token.LoopEnd; Token.LoopStart])
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, SyntaxChecker().CheckSyntax [Token.LoopStart; Token.IncrementData; Token.LoopEnd; Token.IncrementData; Token.LoopStart; Token.IncrementData])