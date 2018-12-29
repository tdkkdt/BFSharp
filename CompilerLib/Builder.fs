namespace CompilerLib

open System.Reflection.Emit
open System.Reflection

type internal LoopInfo = {BodyLabel : Label; ConditionLabel : Label}
type internal BuilderContext = {IlGenerator : ILGenerator; MemoryFI : FieldInfo; PtrFI : FieldInfo; LoopInfos : LoopInfo list}

type Builder() =
    let OnlyOnce fn =
        let mutable value = None
        let resultFn = 
            match value with
            | Some(x) -> x
            | None -> value <- Some(fn); value.Value
        resultFn

    let ReadMI = OnlyOnce (typedefof<System.Console>.GetMethod("Read"))

    let WriteMI = OnlyOnce (typedefof<System.Console>.GetMethod("Write", [|typedefof<char>|]))

    let prorcessGetPtr context = 
        context.IlGenerator.Emit(OpCodes.Ldarg_0)
        context.IlGenerator.Emit(OpCodes.Ldfld, context.PtrFI)

    let processGetMemory context = 
        context.IlGenerator.Emit(OpCodes.Ldarg_0)
        context.IlGenerator.Emit(OpCodes.Ldfld, context.MemoryFI)

    let processPointerOperation context opCode processNextTokenFn =
        context.IlGenerator.Emit(OpCodes.Ldarg_0)
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldc_I4_1)
        context.IlGenerator.Emit(opCode)
        context.IlGenerator.Emit(OpCodes.Stfld, context.PtrFI)
        processNextTokenFn context

    let processDataOperation context opCode processNextTokenFn =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelema, typedefof<byte>)
        context.IlGenerator.Emit(OpCodes.Dup)
        context.IlGenerator.Emit(OpCodes.Ldind_U1)
        context.IlGenerator.Emit(OpCodes.Ldc_I4_1)
        context.IlGenerator.Emit(opCode)
        context.IlGenerator.Emit(OpCodes.Conv_U1)
        context.IlGenerator.Emit(OpCodes.Stind_I1)
        processNextTokenFn context

    let processIncrementPtr context = processPointerOperation context OpCodes.Add

    let processDecrementPtr context = processPointerOperation context OpCodes.Sub

    let processIncrementData context = processDataOperation context OpCodes.Add

    let processDecrementData context = processDataOperation context OpCodes.Sub

    let processInputData context processNextTokenFn =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Call, ReadMI)
        context.IlGenerator.Emit(OpCodes.Conv_U1)
        context.IlGenerator.Emit(OpCodes.Stelem_I1)
        processNextTokenFn context

    let processOutputData context processNextTokenFn =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelem_U1)
        context.IlGenerator.Emit(OpCodes.Call, WriteMI)
        processNextTokenFn context
    
    let processLoopStart context processNextTokensFn = 
        let bodyLabel = context.IlGenerator.DefineLabel()
        let conditionLabel = context.IlGenerator.DefineLabel()
        context.IlGenerator.Emit(OpCodes.Br, conditionLabel)
        context.IlGenerator.MarkLabel bodyLabel
        processNextTokensFn {context with LoopInfos = {BodyLabel = bodyLabel; ConditionLabel = conditionLabel} :: context.LoopInfos}

    let processLoopEnd context processNextTokensFn = 
        let loopInfo = context.LoopInfos.Head
        context.IlGenerator.MarkLabel loopInfo.ConditionLabel
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelem_U1)
        context.IlGenerator.Emit(OpCodes.Brtrue, loopInfo.BodyLabel)
        processNextTokensFn {context with LoopInfos = context.LoopInfos.Tail}

    let processToken (token : Token) (context : BuilderContext) (processNextTokenFn : BuilderContext -> unit) =
        match token with
          | Token.IncrementPtr -> processIncrementPtr context processNextTokenFn
          | Token.DecrementPtr -> processDecrementPtr context processNextTokenFn
          | Token.IncrementData -> processIncrementData context processNextTokenFn
          | Token.DecrementData -> processDecrementData context processNextTokenFn
          | Token.InputData -> processInputData context processNextTokenFn
          | Token.OutputData -> processOutputData context processNextTokenFn
          | Token.LoopStart -> processLoopStart context processNextTokenFn
          | Token.LoopEnd -> processLoopEnd context processNextTokenFn
          | Token.Unknown -> processNextTokenFn context
    
    let rec processNextToken (tokens : Token list) (context : BuilderContext) =
        match tokens with
          | h::t -> processToken h context (processNextToken t)
          | [] -> ()

    member this.Build (tokens : Token list) (ilGenerator : ILGenerator) (memoryFI : FieldInfo) (ptrFI : FieldInfo) (doOptimizations : bool) =
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldc_I4, 30000)
        ilGenerator.Emit(OpCodes.Newarr, typedefof<byte>)
        ilGenerator.Emit(OpCodes.Stfld, memoryFI)
        processNextToken tokens {IlGenerator = ilGenerator; MemoryFI = memoryFI; PtrFI = ptrFI; LoopInfos = []}
        ilGenerator.Emit(OpCodes.Ret)