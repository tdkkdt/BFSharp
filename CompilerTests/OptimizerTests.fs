namespace Tests

open NUnit.Framework
open CompilerLib

[<OptimizerTests>]
type OptimizerTests () =

    [<Test>]
    member this.OptimizePointerOperations() =
        Assert.AreEqual([Token.MovePtrDelta(3)], Optimizer.optimizeTokens [Token.IncrementPtr; Token.IncrementPtr; Token.IncrementPtr])
        Assert.AreEqual([Token.MovePtrDelta(1)], Optimizer.optimizeTokens [Token.IncrementPtr; Token.DecrementPtr; Token.IncrementPtr])
        Assert.AreEqual([
                Token.SetDataDelta(1); 
                Token.LoopStart; 
                Token.MovePtrDelta(1);
                Token.LoopEnd
            ], 
            Optimizer.optimizeTokens [
                Token.IncrementData; 
                Token.LoopStart; 
                Token.IncrementPtr; 
                Token.DecrementPtr; 
                Token.IncrementPtr; 
                Token.LoopEnd
            ]
        )

    [<Test>]
    member this.OptimizeDataOperations() =
        Assert.AreEqual([Token.SetDataDelta(3)], Optimizer.optimizeTokens [Token.IncrementData; Token.IncrementData; Token.IncrementData])
        Assert.AreEqual([Token.SetDataDelta(1)], Optimizer.optimizeTokens [Token.IncrementData; Token.DecrementData; Token.IncrementData])
        Assert.AreEqual([
                Token.OutputData;
                Token.SetDataDelta(1);
                Token.OutputData;
            ], 
            Optimizer.optimizeTokens [
                Token.OutputData;
                Token.IncrementData; 
                Token.DecrementData; 
                Token.IncrementData; 
                Token.OutputData;
            ]
        )

    [<Test>]
    member this.OptimizeClearDataOperations() =
        Assert.AreEqual(
            [Token.SetDataDelta(1); Token.OutputData; Token.ClearData], 
            Optimizer.optimizeTokens [Token.IncrementData; Token.OutputData; Token.LoopStart; Token.DecrementData; Token.LoopEnd]
        )
        Assert.AreEqual(
            [Token.SetDataDelta(1); Token.OutputData; Token.ClearData], 
            Optimizer.optimizeTokens [Token.IncrementData; Token.OutputData; Token.LoopStart; Token.IncrementData; Token.LoopEnd]
        )