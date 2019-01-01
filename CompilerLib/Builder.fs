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

    let rec private processPointerOperation nextTokens context opCode =
        context.IlGenerator.Emit(OpCodes.Ldarg_0)
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldc_I4_1)
        context.IlGenerator.Emit(opCode)
        context.IlGenerator.Emit(OpCodes.Stfld, context.PtrFI)
        processNextToken nextTokens context

    and private processDataOperation nextTokens context opCode =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelema, typedefof<byte>)
        context.IlGenerator.Emit(OpCodes.Dup)
        context.IlGenerator.Emit(OpCodes.Ldind_U1)
        context.IlGenerator.Emit(OpCodes.Ldc_I4_1)
        context.IlGenerator.Emit(opCode)
        context.IlGenerator.Emit(OpCodes.Conv_U1)
        context.IlGenerator.Emit(OpCodes.Stind_I1)
        processNextToken nextTokens context

    and private processIncrementPtr nextTokens context = processPointerOperation nextTokens context OpCodes.Add

    and private processDecrementPtr nextTokens context = processPointerOperation nextTokens context OpCodes.Sub

    and private processIncrementData nextTokens context = processDataOperation nextTokens context OpCodes.Add

    and private processDecrementData nextTokens context = processDataOperation nextTokens context OpCodes.Sub

    and private processInputData nextTokens context =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Call, ReadMI)
        context.IlGenerator.Emit(OpCodes.Conv_U1)
        context.IlGenerator.Emit(OpCodes.Stelem_I1)
        processNextToken nextTokens context

    and private processOutputData nextTokens context =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelem_U1)
        context.IlGenerator.Emit(OpCodes.Call, WriteMI)
        processNextToken nextTokens context
    
    and private processLoopStart nextTokens context = 
        let bodyLabel = context.IlGenerator.DefineLabel()
        let conditionLabel = context.IlGenerator.DefineLabel()
        context.IlGenerator.Emit(OpCodes.Br, conditionLabel)
        context.IlGenerator.MarkLabel bodyLabel
        processNextToken nextTokens {context with LoopInfos = {BodyLabel = bodyLabel; ConditionLabel = conditionLabel} :: context.LoopInfos}

    and private processLoopEnd nextTokens context = 
        let loopInfo = context.LoopInfos.Head
        context.IlGenerator.MarkLabel loopInfo.ConditionLabel
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelem_U1)
        context.IlGenerator.Emit(OpCodes.Brtrue, loopInfo.BodyLabel)
        processNextToken nextTokens {context with LoopInfos = context.LoopInfos.Tail}

    and private processToken (token : Token) (context : BuilderContext) (nextTokens :Token list) =
        match token with
          | Token.IncrementPtr -> processIncrementPtr nextTokens context
          | Token.DecrementPtr -> processDecrementPtr nextTokens context
          | Token.IncrementData -> processIncrementData nextTokens context
          | Token.DecrementData -> processDecrementData nextTokens context
          | Token.InputData -> processInputData nextTokens context
          | Token.OutputData -> processOutputData nextTokens context
          | Token.LoopStart -> processLoopStart nextTokens context
          | Token.LoopEnd -> processLoopEnd nextTokens context
          | Token.Unknown -> processNextToken nextTokens context
    
    and private processNextToken (tokens : Token list) (context : BuilderContext) =
        match tokens with
          | [] -> ()
          | h::t -> processToken h context t

    let build (tokens : Token list) (ilGenerator : ILGenerator) (memoryFI : FieldInfo) (ptrFI : FieldInfo) (doOptimizations : bool) =
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldc_I4, 30000)
        ilGenerator.Emit(OpCodes.Newarr, typedefof<byte>)
        ilGenerator.Emit(OpCodes.Stfld, memoryFI)
        processNextToken tokens {IlGenerator = ilGenerator; MemoryFI = memoryFI; PtrFI = ptrFI; LoopInfos = []}
        ilGenerator.Emit(OpCodes.Ret)