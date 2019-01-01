namespace Tests

open System
open NUnit.Framework
open CompilerLib
open System.Threading
open System.Reflection
open System.Reflection.Emit
open System.IO
open System.Diagnostics

type TestCodeContainerBase () =
    [<DefaultValue>] val mutable public ptr : int
    [<DefaultValue>] val mutable public memory : byte array

[<CompileTests>]
type CompileTests () =

    let compile tokens = 
        let testAssemblyName = AssemblyName("TestAssembly")
        let currentDom = Thread.GetDomain()
        let assemblyBuilder = currentDom.DefineDynamicAssembly(testAssemblyName, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule("TestModule")
        let testCodeContainerBaseType = typedefof<TestCodeContainerBase>
        let typeBuilder = moduleBuilder.DefineType("TestCodeContainer", TypeAttributes.Public ||| TypeAttributes.Class, testCodeContainerBaseType)
        let baseConstructor = testCodeContainerBaseType.GetConstructor([||])
        let constructorBuilder = typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
        let constructorIlGenerator = constructorBuilder.GetILGenerator()
        constructorIlGenerator.Emit(OpCodes.Ldarg_0)
        constructorIlGenerator.Emit(OpCodes.Call, baseConstructor)
        constructorIlGenerator.Emit(OpCodes.Ret)
        let methodBuilder = typeBuilder.DefineMethod("Main", MethodAttributes.Public, typedefof<System.Void>, null)
        let ilGenerator = methodBuilder.GetILGenerator()
        let ptrFI = testCodeContainerBaseType.GetField("ptr")
        let memoryFI = testCodeContainerBaseType.GetField ("memory")
        Builder.build tokens ilGenerator memoryFI ptrFI
        let testCodeContainerType = typeBuilder.CreateTypeInfo().AsType()
        let testCodeContainer = Activator.CreateInstance(testCodeContainerType)
        testCodeContainerType.InvokeMember("Main", BindingFlags.InvokeMethod, null, testCodeContainer, [||]) |> ignore
        testCodeContainer:?> TestCodeContainerBase

    [<Test>]
    member this.InitTest() =
        let codeContainer = compile []
        Assert.AreEqual(0, codeContainer.ptr)
        Assert.AreEqual(30000, codeContainer.memory.Length)
        Assert.IsTrue(codeContainer.memory |> Array.forall((=) 0uy))

    [<Test>]
    member this.IncrementPtrCompileTest () =
        let codeContainer = compile [Token.IncrementPtr]
        Assert.AreEqual(1, codeContainer.ptr)

    [<Test>]
    member this.DecrementPtrCompileTest () =
        let codeContainer = compile [Token.DecrementPtr]
        Assert.AreEqual(-1, codeContainer.ptr)

    [<Test>]
    member this.IncrementDataCompileTest () =
        let codeContainer = compile [Token.IncrementData]
        Assert.AreEqual(1, codeContainer.memory.[0])

    [<Test>]
    member this.DecrementDataCompileTest () =
        let codeContainer = compile [Token.DecrementData]
        Assert.AreEqual(255, codeContainer.memory.[0])

    [<Test>]
    member this.OutputDataCompileTest() = 
        let stream = new MemoryStream(1)
        let testOut = new StreamWriter(stream)
        Console.SetOut(testOut)
        let codeContainer = compile [Token.IncrementData; Token.OutputData]
        testOut.Flush()
        stream.Position <- stream.Position - 1L
        Assert.AreEqual(1, stream.ReadByte())
        testOut.Dispose()
        stream.Dispose()

    [<Test>]
    member this.InputDataCompileTest() = 
        let stream = new MemoryStream([|26uy|])
        let testIn = new StreamReader(stream)
        Console.SetIn(testIn)
        let codeContainer = compile [Token.InputData]
        Assert.AreEqual(26uy, codeContainer.memory.[0])
        testIn.Dispose()
        stream.Dispose()

    [<Test>]
    member this.LoopCompileTest() = 
        let codeContainer = compile [
            Token.IncrementData;
            Token.IncrementData;
            Token.LoopStart;
            Token.IncrementPtr;
            Token.IncrementData;
            Token.IncrementData;
            Token.DecrementPtr;
            Token.DecrementData;
            Token.LoopEnd
        ]
        Assert.AreEqual(0uy, codeContainer.memory.[0])
        Assert.AreEqual(4uy, codeContainer.memory.[1])

    [<Test>]
    member this.MovePtrDeltaCompileTest1 () =
        let codeContainer = compile [Token.IncrementPtr; Token.IncrementPtr; Token.DecrementPtr; Token.IncrementPtr;]
        Assert.AreEqual(2, codeContainer.ptr)

    [<Test>]
    member this.MovePtrDeltaCompileTest2 () =
        let codeContainer = compile [Token.IncrementPtr; Token.IncrementData; Token.IncrementPtr; Token.DecrementPtr; Token.DecrementPtr;]
        Assert.AreEqual(0, codeContainer.ptr)

    [<Test>]
    member this.MovePtrDeltaCompileTest3 () =
        let codeContainer = compile [Token.MovePtrDelta 10000]
        Assert.AreEqual(10000, codeContainer.ptr)

    [<Test>]
    member this.SetDataDeltaCompileTest1 () =
        let codeContainer = compile [Token.IncrementData; Token.IncrementData; Token.DecrementData; Token.IncrementData;]
        Assert.AreEqual(2, codeContainer.memory.[0])

    [<Test>]
    member this.SetDataDeltaCompileTest2 () =
        let codeContainer = compile [Token.IncrementData; Token.IncrementPtr; Token.IncrementData; Token.DecrementData; Token.DecrementData;]
        Assert.AreEqual(255, codeContainer.memory.[1])

    [<Test>]
    member this.SetDataDeltaCompileTest3 () =
        let codeContainer = compile [Token.SetDataDelta 247]
        Assert.AreEqual(247, codeContainer.memory.[0])

    [<Test>]
    member this.ClearDataTest1 () =
        let codeContainer = compile [Token.SetDataDelta 247; Token.ClearData]
        Assert.AreEqual(0, codeContainer.memory.[0])