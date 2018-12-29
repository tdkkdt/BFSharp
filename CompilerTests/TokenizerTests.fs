namespace Tests

open NUnit.Framework
open CompilerLib

[<TokenizerTests>]
type TokenizerTests () =

    [<Test>]
    member this.SimpleTest () =
        Assert.AreEqual([Token.IncrementPtr], Tokens().Tokenize ">");
        Assert.AreEqual([Token.DecrementPtr], Tokens().Tokenize "<");
        Assert.AreEqual([Token.IncrementData], Tokens().Tokenize "+");
        Assert.AreEqual([Token.DecrementData], Tokens().Tokenize "-");
        Assert.AreEqual([Token.OutputData], Tokens().Tokenize ".");
        Assert.AreEqual([Token.InputData], Tokens().Tokenize ",");
        Assert.AreEqual([Token.LoopStart], Tokens().Tokenize "[");
        Assert.AreEqual([Token.LoopEnd], Tokens().Tokenize "]");

    [<Test>]
    member this.SeveralTokens() = 
        Assert.AreEqual(
            [
                Token.IncrementPtr;
                Token.DecrementPtr;
                Token.IncrementData;
                Token.DecrementData;
                Token.OutputData;
                Token.InputData;
                Token.LoopStart;
                Token.LoopEnd
            ],
            Tokens().Tokenize "><+-.,[]"
        );

    [<Test>]
    member this.TokensWithComments() = 
        Assert.AreEqual(
            [
                Token.LoopStart;
                Token.LoopEnd
                Token.IncrementPtr;
                Token.DecrementPtr;
                Token.IncrementData;
                Token.DecrementData;
                Token.OutputData;
                Token.InputData;
                Token.LoopStart;
                Token.LoopEnd
            ],
            Tokens().Tokenize "[Comment]><+-\r\nabc.,[]"
        );