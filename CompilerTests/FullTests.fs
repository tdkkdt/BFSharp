namespace Tests

open NUnit.Framework
open System
open System.IO
open CompilerLib
open System.Diagnostics
open System.Reflection
open System.Reflection.Emit

[<FullTests>]
type FullTests() =

    [<Test>]
    member this.TestAllFiles() =
        let filesPath = Path.Combine(System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName, @"..\..\Files\")
        Directory.EnumerateFiles(filesPath, "*.b") 
        |> Seq.iter (fun fileName ->
            let fileContent = File.ReadAllText fileName
            let tokens = Tokens().Tokenize fileContent
            let syntaxCheckResult = SyntaxChecker().CheckSyntax tokens
            Assert.AreEqual(SyntaxCheckResult.OK, syntaxCheckResult)
            let assembly = Compiler().CreateDynamicAssembly tokens "test.exe" AssemblyBuilderAccess.RunAndCollect
            let codeContainerType = assembly.EntryPoint.DeclaringType
            let codeContainer = Activator.CreateInstance(codeContainerType)
            let onlyFileName = Path.GetFileNameWithoutExtension fileName
            let inputFile = filesPath + onlyFileName + ".in"
            let outputFile = filesPath + onlyFileName + ".out"
            let inputMemoryStream = new MemoryStream(if File.Exists(inputFile) then File.ReadAllBytes(inputFile) else [||])
            let input = new StreamReader(inputMemoryStream)
            let outputMemoryStream = new MemoryStream(if File.Exists(outputFile) then FileInfo(outputFile).Length |> int else 0)
            let output = new StreamWriter(outputMemoryStream)
            Console.SetOut(output)
            Console.SetIn(input)
            //assembly.EntryPoint.Invoke(codeContainer, [||]) |> ignore
            codeContainerType.InvokeMember("Main", BindingFlags.InvokeMethod, null, codeContainer, [||]) |> ignore
            output.Flush()
            let actualOutput = outputMemoryStream.ToArray()
            let expectedOutput = if File.Exists(outputFile) then File.ReadAllBytes(outputFile) else [||]
            Assert.AreEqual(expectedOutput.Length, actualOutput.Length)
            Assert.AreEqual(0, Array.compareWith compare expectedOutput actualOutput)
            output.Dispose()
            outputMemoryStream.Dispose()
            input.Dispose()
            inputMemoryStream.Dispose()
        )