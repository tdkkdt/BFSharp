namespace CompilerLib

open System.Reflection.Emit
open System.Reflection

module Builder =
    type internal LoopInfo = {BodyLabel : Label; ConditionLabel : Label}
    type internal BuilderContext = {IlGenerator : ILGenerator; MemoryFI : FieldInfo; PtrFI : FieldInfo; LoopInfos : LoopInfo list}

    let private OnlyOnce fn =
        let mutable value = None
        let resultFn = 
            match value with
            | Some(x) -> x
            | None -> value <- Some(fn); value.Value
        resultFn

    let private ReadMI = OnlyOnce (typedefof<System.Console>.GetMethod("Read"))

    let private WriteMI = OnlyOnce (typedefof<System.Console>.GetMethod("Write", [|typedefof<char>|]))

    let private prorcessGetPtr context = 
        context.IlGenerator.Emit(OpCodes.Ldarg_0)
        context.IlGenerator.Emit(OpCodes.Ldfld, context.PtrFI)

    let private processGetMemory context = 
        context.IlGenerator.Emit(OpCodes.Ldarg_0)
        context.IlGenerator.Emit(OpCodes.Ldfld, context.MemoryFI)

    let private processPointerOperation context opCode processNextTokenFn =
        context.IlGenerator.Emit(OpCodes.Ldarg_0)
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldc_I4_1)
        context.IlGenerator.Emit(opCode)
        context.IlGenerator.Emit(OpCodes.Stfld, context.PtrFI)
        processNextTokenFn context

    let private processDataOperation context opCode processNextTokenFn =
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

    let private processIncrementPtr context = processPointerOperation context OpCodes.Add

    let private processDecrementPtr context = processPointerOperation context OpCodes.Sub

    let private processIncrementData context = processDataOperation context OpCodes.Add

    let private processDecrementData context = processDataOperation context OpCodes.Sub

    let private processInputData context processNextTokenFn =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Call, ReadMI)
        context.IlGenerator.Emit(OpCodes.Conv_U1)
        context.IlGenerator.Emit(OpCodes.Stelem_I1)
        processNextTokenFn context

    let private processOutputData context processNextTokenFn =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelem_U1)
        context.IlGenerator.Emit(OpCodes.Call, WriteMI)
        processNextTokenFn context
    
    let private processLoopStart context processNextTokensFn = 
        let bodyLabel = context.IlGenerator.DefineLabel()
        let conditionLabel = context.IlGenerator.DefineLabel()
        context.IlGenerator.Emit(OpCodes.Br, conditionLabel)
        context.IlGenerator.MarkLabel bodyLabel
        processNextTokensFn {context with LoopInfos = {BodyLabel = bodyLabel; ConditionLabel = conditionLabel} :: context.LoopInfos}

    let private processLoopEnd context processNextTokensFn = 
        let loopInfo = context.LoopInfos.Head
        context.IlGenerator.MarkLabel loopInfo.ConditionLabel
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelem_U1)
        context.IlGenerator.Emit(OpCodes.Brtrue, loopInfo.BodyLabel)
        processNextTokensFn {context with LoopInfos = context.LoopInfos.Tail}

    let rec private processToken (token : Token) (context : BuilderContext) (nextTokens :Token list) =
        match token with
          | Token.IncrementPtr -> processIncrementPtr context (processNextToken nextTokens)
          | Token.DecrementPtr -> processDecrementPtr context (processNextToken nextTokens)
          | Token.IncrementData -> processIncrementData context (processNextToken nextTokens)
          | Token.DecrementData -> processDecrementData context (processNextToken nextTokens)
          | Token.InputData -> processInputData context (processNextToken nextTokens)
          | Token.OutputData -> processOutputData context (processNextToken nextTokens)
          | Token.LoopStart -> processLoopStart context (processNextToken nextTokens)
          | Token.LoopEnd -> processLoopEnd context (processNextToken nextTokens)
          | Token.Unknown -> processNextToken nextTokens context
    
    and private processNextToken (tokens : Token list) (context : BuilderContext) =
        match tokens with
          | [] -> ()
          | h::t -> processToken h context t |> ignore

    let build (tokens : Token list) (ilGenerator : ILGenerator) (memoryFI : FieldInfo) (ptrFI : FieldInfo) (doOptimizations : bool) =
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldc_I4, 30000)
        ilGenerator.Emit(OpCodes.Newarr, typedefof<byte>)
        ilGenerator.Emit(OpCodes.Stfld, memoryFI)
        processNextToken tokens {IlGenerator = ilGenerator; MemoryFI = memoryFI; PtrFI = ptrFI; LoopInfos = []}
        ilGenerator.Emit(OpCodes.Ret)