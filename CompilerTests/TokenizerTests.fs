namespace Tests

open NUnit.Framework
open CompilerLib

[<TokenizerTests>]
type TokenizerTests () =

    [<Test>]
    member this.SimpleTest () =
        Assert.AreEqual([Token.IncrementPtr], Tokens.tokenize ">");
        Assert.AreEqual([Token.DecrementPtr], Tokens.tokenize "<");
        Assert.AreEqual([Token.IncrementData], Tokens.tokenize "+");
        Assert.AreEqual([Token.DecrementData], Tokens.tokenize "-");
        Assert.AreEqual([Token.OutputData], Tokens.tokenize ".");
        Assert.AreEqual([Token.InputData], Tokens.tokenize ",");
        Assert.AreEqual([Token.LoopStart], Tokens.tokenize "[");
        Assert.AreEqual([Token.LoopEnd], Tokens.tokenize "]");

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
            Tokens.tokenize "><+-.,[]"
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
            Tokens.tokenize "[Comment]><+-\r\nabc.,[]"
        );