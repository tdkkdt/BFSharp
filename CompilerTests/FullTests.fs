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

    let testFile (fileName : string) (expectedOutput : byte array) =
        let filesPath = Path.Combine(System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName, @"..\..\Files\")
        let filePath = Path.Combine(filesPath, fileName + ".b")
        let fileContent = File.ReadAllText filePath
        let tokens = Tokens.tokenize fileContent
        let syntaxCheckResult = SyntaxChecker.checkSyntax tokens
        Assert.AreEqual(SyntaxCheckResult.OK, syntaxCheckResult)
        let assembly = Compiler.createDynamicAssembly tokens "test.exe" AssemblyBuilderAccess.RunAndCollect
        let codeContainerType = assembly.EntryPoint.DeclaringType
        let codeContainer = Activator.CreateInstance(codeContainerType)
        let onlyFileName = Path.GetFileNameWithoutExtension filePath
        let inputFile = filesPath + onlyFileName + ".in"
        let inputMemoryStream = new MemoryStream(if File.Exists(inputFile) then File.ReadAllBytes(inputFile) else [||])
        let input = new StreamReader(inputMemoryStream)
        let outputMemoryStream = new MemoryStream(expectedOutput.Length)
        let output = new StreamWriter(outputMemoryStream)
        Console.SetOut(output)
        Console.SetIn(input)
        try 
            codeContainerType.InvokeMember("Main", BindingFlags.InvokeMethod, null, codeContainer, [||]) |> ignore
        with 
            | :? System.Reflection.TargetInvocationException as ex when ex.InnerException <> null -> raise ex.InnerException
            | _ -> reraise()
        output.Flush()
        let actualOutput = outputMemoryStream.ToArray()
        Assert.AreEqual(expectedOutput.Length, actualOutput.Length)
        Assert.AreEqual(0, Array.compareWith compare expectedOutput actualOutput)
        output.Dispose()
        outputMemoryStream.Dispose()
        input.Dispose()
        inputMemoryStream.Dispose()

    let readOuputFromFile fileName = 
        let filesPath = Path.Combine(System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName, @"..\..\Files\")
        let filePath = Path.Combine(filesPath, fileName + ".out")
        File.ReadAllBytes(filePath)

    let readOutputFromString (value : string) = value.ToCharArray() |> Array.map byte

    let byteArrayToString (bytes:byte array) : string = System.String(bytes |> Array.map char)

    [<Test; Timeout(5000)>]
    member this.Alcount1Test() =
        testFile "al-count-1" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Alcount2Test() =
        testFile "al-count-2" (readOutputFromString "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

    [<Test; Timeout(5000)>]
    member this.Awib04() =
        testFile "awib-0.4" (readOuputFromFile "awib-0.4")

    [<Test; Timeout(5000)>]
    member this.Beer() =
        testFile "Beer" (readOuputFromFile "Beer")

    [<Test; Timeout(5000)>]
    member this.Bench() =
        testFile "Bench" (readOuputFromFile "Bench")

    [<Test; Timeout(5000)>]
    member this.BusyBeaver() =
        testFile "BusyBeaver" (readOuputFromFile "BusyBeaver")

    [<Test; Timeout(5000)>]
    member this.Cells30k() =
        testFile "cells30k" (readOuputFromFile "cells30k")

    [<Test; Timeout(5000)>]
    member this.Cellsize() =
        testFile "Cellsize" (readOuputFromFile "Cellsize")

    [<Test; Timeout(5000)>]
    member this.Cellsize2() =
        testFile "Cellsize2" (readOutputFromString "This interpreter has 8bit cells.\n")

    [<Test; Timeout(5000)>]
    member this.Cellsize3() =
        testFile "Cellsize3" (readOuputFromFile "Cellsize3")

    [<Test; Timeout(5000)>]
    member this.Cellsize4() =
        testFile "Cellsize4" (readOutputFromString "This interpreter has 8 bit cells.\n")

    [<Test; Timeout(5000)>]
    member this.Cellsize5() =
        testFile "Cellsize5" (readOuputFromFile "Cellsize5")

    [<Test; Timeout(5000)>]
    member this.Chess() =
        testFile "chess" (readOuputFromFile "Chess")

    [<Test; Timeout(5000)>]
    member this.Collatz() =
        testFile "Collatz" (readOuputFromFile "Collatz")

    [<Test; Timeout(5000)>]
    member this.Counter() =
        testFile "Counter" (readOuputFromFile "Counter")

    [<Test; Timeout(5000)>]
    member this.Cristofd30000() =
        testFile "cristofd-30000" (readOutputFromString "#\n")

    [<Test; Timeout(5000)>]
    member this.Cristofdclose() =
        let filesPath = Path.Combine(System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName, @"..\..\Files\")
        let filePath = Path.Combine(filesPath, "cristofd-close.b")
        let fileContent = File.ReadAllText filePath
        let tokens = Tokens.tokenize fileContent
        let syntaxCheckResult = SyntaxChecker.checkSyntax tokens
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, syntaxCheckResult)

    [<Test; Timeout(5000)>]
    member this.Cristofdendtest() =
        testFile "cristofd-endtest" (readOutputFromString "LA\nLA\n")

    [<Test; Timeout(5000)>]
    member this.Cristofdleftmargin() =
        Assert.Throws<System.IndexOutOfRangeException> (fun () -> (testFile "cristofd-leftmargin" (readOuputFromFile "al-count-1"))) |> ignore

    [<Test; Timeout(5000)>] 
    member this.Cristofdmisctest() =
        testFile "cristofd-misctest" (readOutputFromString "H\n")

    [<Test; Timeout(5000)>]
    member this.Cristofdopen() =
        let filesPath = Path.Combine(System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName, @"..\..\Files\")
        let filePath = Path.Combine(filesPath, "cristofd-open.b")
        let fileContent = File.ReadAllText filePath
        let tokens = Tokens.tokenize fileContent
        let syntaxCheckResult = SyntaxChecker.checkSyntax tokens
        Assert.AreEqual(SyntaxCheckResult.LoopsProblem, syntaxCheckResult)

    [<Test; Timeout(5000)>]
    member this.Cristofdrightmargin() =
        Assert.Throws<System.IndexOutOfRangeException> (fun () -> (testFile "cristofd-rightmargin" (readOuputFromFile "al-count-1"))) |> ignore

    [<Test; Timeout(5000)>]
    member this.EasyOpt() =
        testFile "EasyOpt" (readOutputFromString "OK\n")

    [<Test; Timeout(5000)>]
    member this.Endtest() =
        testFile "Endtest" (readOutputFromString "<NL>\n0xFF\n")

    [<Test; Timeout(5000)>]
    member this.Euler1() =
        testFile "Euler1" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Euler5() =
        testFile "Euler5" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Factor() =
        testFile "Factor" (readOuputFromFile "Factor")

    [<Test; Timeout(5000)>]
    member this.Golden() =
        testFile "Golden" (readOuputFromFile "Golden")

    [<Test; Timeout(5000)>]
    member this.Hanoi() =
        testFile "Hanoi" (readOuputFromFile "Hanoi")

    [<Test; Timeout(5000)>]
    member this.Hello() =
        testFile "Hello" (readOuputFromFile "Hello")

    [<Test; Timeout(5000)>]
    member this.Hello2() =
        testFile "Hello2" (readOuputFromFile "Hello2")

    [<Test; Timeout(5000)>]
    member this.Impeccable() =
        testFile "Impeccable" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Kiloseconds () =
        testFile "Kiloseconds" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Life() =
        testFile "Life" (readOuputFromFile "Life")

    [<Test; Timeout(5000)>]
    member this.Long() =
        testFile "Long" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000); Ignore("Stackoverflow")>]
    member this.LostKng() =
        testFile "LostKng" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Mandelbrot() =
        testFile "Mandelbrot" (readOuputFromFile "Mandelbrot")

    [<Test; Timeout(5000)>]
    member this.Mandelbrotextreme() =
        testFile "Mandelbrot-extreme" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Mandelbrottiny() =
        testFile "Mandelbrot-tiny" (readOuputFromFile "Mandelbrot-tiny")

    [<Test; Timeout(5000)>]
    member this.OptimTease() =
        testFile "OptimTease" (readOuputFromFile "OptimTease")

    [<Test; Timeout(5000)>]
    member this.PIdigits() =
        testFile "PIdigits" (readOuputFromFile "PIdigits")

    [<Test; Timeout(5000)>]
    member this.PIdigitsas() =
        testFile "PIdigits-as" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.PIdigitscp() =
        testFile "PIdigits-cp" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.PIdigitsorig() =
        testFile "PIdigits-orig" (readOutputFromString "3.141\n")

    [<Test; Timeout(5000)>]
    member this.PIdigits16() =
        testFile "PIdigits.16" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Precalc() =
        testFile "Precalc" (readOutputFromString "Large byte doubled code\n")

    [<Test; Timeout(5000)>]
    member this.Prime() =
        testFile "Prime" (readOuputFromFile "Prime")

    [<Test; Timeout(5000)>]
    member this.Primedoubled() =
        testFile "Prime-doubled" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Primeorig() =
        testFile "Prime-orig" (readOuputFromFile "Prime-orig")

    [<Test; Timeout(5000)>]
    member this.Prttab() =
        testFile "Prttab" (readOuputFromFile "Prttab")

    [<Test; Timeout(5000)>]
    member this.SelfInt() =
        testFile "SelfInt" (readOuputFromFile "SelfInt")

    [<Test; Timeout(5000)>]
    member this.Skiploop() =
        testFile "Skiploop" (readOutputFromString "OK\n")

    [<Test; Timeout(5000)>]
    member this.Tribit() =
        testFile "Tribit" (readOutputFromString "8 bit cells\n")

    [<Test; Timeout(5000)>]
    member this.Utm() =
        testFile "utm" (readOutputFromString "11111111111111111c11111111111111111c11111111111111111c11111111111111111c11111111111111111c1c111111111111111111111\n")

    [<Test; Timeout(5000); Ignore("StackOverflow")>]
    member this.Zozotez() =
        testFile "Zozotez" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000); Ignore("StackOverflow")>]
    member this.Zozotezdoubled() =
        testFile "Zozotez-doubled" (readOuputFromFile "al-count-1")

    [<Test; Timeout(5000)>]
    member this.Zozotezorig() =
        testFile "Zozotez-orig" (readOuputFromFile "al-count-1")