namespace CompilerLib

open System
open System.Threading;
open System.Reflection;
open System.Reflection.Emit;

module Compiler =  
    let createDynamicAssembly (tokens : Token list) (exeFileName : string) (access : AssemblyBuilderAccess): AssemblyBuilder =
        let shortFileName = System.IO.Path.GetFileName exeFileName
        let assemblyName = AssemblyName(shortFileName)
        let currentDom = Thread.GetDomain()
        let assemblyBuilder = currentDom.DefineDynamicAssembly(assemblyName, access)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(shortFileName, shortFileName)
        let codeContainerBaseType = typedefof<System.Object>
        let baseConstructor = codeContainerBaseType.GetConstructor([||])

        let codeContainerTypeBuilder = moduleBuilder.DefineType("CodeContainer", TypeAttributes.NotPublic ||| TypeAttributes.Class, codeContainerBaseType)
        let codeContainerConstructorBuilder = codeContainerTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
        let codeContainerConstructorIlGenerator = codeContainerConstructorBuilder.GetILGenerator()
        codeContainerConstructorIlGenerator.Emit(OpCodes.Ldarg_0)
        codeContainerConstructorIlGenerator.Emit(OpCodes.Call, baseConstructor)
        codeContainerConstructorIlGenerator.Emit(OpCodes.Ret)
        let codeContainerMainMethodBuilder = codeContainerTypeBuilder.DefineMethod("Main", MethodAttributes.Public, typedefof<System.Void>, null)
        let codeContainerIlGenerator = codeContainerMainMethodBuilder.GetILGenerator()
        let ptrFI = codeContainerTypeBuilder.DefineField("ptr", typedefof<int>, FieldAttributes.Private)
        let memoryFI = codeContainerTypeBuilder.DefineField("memory", typedefof<byte[]>, FieldAttributes.Private)
        Builder.build tokens codeContainerIlGenerator memoryFI ptrFI

        codeContainerTypeBuilder.CreateType() |> ignore

        let programTypeBuilder = moduleBuilder.DefineType("Program", TypeAttributes.NotPublic ||| TypeAttributes.Class, codeContainerBaseType)
        let programConstructorBuilder = programTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
        let programConstructorIlGenerator = programConstructorBuilder.GetILGenerator()
        programConstructorIlGenerator.Emit(OpCodes.Ldarg_0)
        programConstructorIlGenerator.Emit(OpCodes.Call, baseConstructor)
        programConstructorIlGenerator.Emit(OpCodes.Ret)

        let programMainMethodBuilder = programTypeBuilder.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static, typedefof<System.Void>, null)
        let programMainMethodIlGenerator = programMainMethodBuilder.GetILGenerator()
        programMainMethodIlGenerator.Emit(OpCodes.Newobj, codeContainerConstructorBuilder)
        programMainMethodIlGenerator.Emit(OpCodes.Call, codeContainerMainMethodBuilder)
        programMainMethodIlGenerator.Emit(OpCodes.Ret)

        assemblyBuilder.SetEntryPoint programMainMethodBuilder
        programTypeBuilder.CreateType() |> ignore
        assemblyBuilder

    let compileFromTokens (tokens : Token list) (exeFileName : string) =
        let assemblyBuilder = createDynamicAssembly tokens exeFileName AssemblyBuilderAccess.RunAndSave
        assemblyBuilder.Save exeFileName
    
    let compileFromText (code : string) (exeFileName : string) =
        let tokens = Tokens.tokenize code
        let syntaxCheckResult = SyntaxChecker.checkSyntax tokens
        match syntaxCheckResult with
            | CodeEmpty -> Console.WriteLine "The code is empty"
            | LoopsProblem -> Console.WriteLine "There is problem with loops. Check it please"
            | OK -> compileFromTokens tokens exeFileName

    let compileFromFile (codeFileName : string) (exeFileName : string) =
        compileFromText (System.IO.File.ReadAllText (codeFileName)) exeFileName