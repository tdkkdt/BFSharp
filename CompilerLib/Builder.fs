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

    and private processMovePtrDelta (delta : int) nextTokens context =
        context.IlGenerator.Emit(OpCodes.Ldarg_0)
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldc_I4, delta)
        context.IlGenerator.Emit(OpCodes.Add)
        context.IlGenerator.Emit(OpCodes.Stfld, context.PtrFI)
        processNextToken nextTokens context
    
    and private processSetDataDelta (delta : int) nextTokens context =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldelema, typedefof<byte>)
        context.IlGenerator.Emit(OpCodes.Dup)
        context.IlGenerator.Emit(OpCodes.Ldind_U1)
        context.IlGenerator.Emit(OpCodes.Ldc_I4, delta)
        context.IlGenerator.Emit(OpCodes.Add)
        context.IlGenerator.Emit(OpCodes.Conv_U1)
        context.IlGenerator.Emit(OpCodes.Stind_I1)
        processNextToken nextTokens context

    and private processClearData nextTokens context =
        processGetMemory context
        prorcessGetPtr context
        context.IlGenerator.Emit(OpCodes.Ldc_I4_0)
        context.IlGenerator.Emit(OpCodes.Stelem_I1)
        processNextToken nextTokens context

    and private processToken (token : Token) (context : BuilderContext) (nextTokens :Token list) =
        match token with
          | IncrementPtr -> processIncrementPtr nextTokens context
          | DecrementPtr -> processDecrementPtr nextTokens context
          | IncrementData -> processIncrementData nextTokens context
          | DecrementData -> processDecrementData nextTokens context
          | InputData -> processInputData nextTokens context
          | OutputData -> processOutputData nextTokens context
          | LoopStart -> processLoopStart nextTokens context
          | LoopEnd -> processLoopEnd nextTokens context
          | Unknown -> processNextToken nextTokens context
          | MovePtrDelta(delta) when delta = 1 -> processIncrementPtr nextTokens context
          | MovePtrDelta(delta) when delta = -1 -> processDecrementPtr nextTokens context
          | MovePtrDelta(delta) -> processMovePtrDelta delta nextTokens context
          | SetDataDelta(delta) when delta = 1 -> processIncrementData nextTokens context
          | SetDataDelta(delta) when delta = -1 -> processDecrementData nextTokens context
          | SetDataDelta(delta) -> processSetDataDelta delta nextTokens context
          | ClearData -> processClearData nextTokens context
    
    and private processNextToken (tokens : Token list) (context : BuilderContext) =
        match tokens with
          | [] -> ()
          | h::t -> processToken h context t

    let build (tokens : Token list) (ilGenerator : ILGenerator) (memoryFI : FieldInfo) (ptrFI : FieldInfo) =
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldc_I4, 30000)
        ilGenerator.Emit(OpCodes.Newarr, typedefof<byte>)
        ilGenerator.Emit(OpCodes.Stfld, memoryFI)
        let optimizedTokens = Optimizer.optimizeTokens tokens
        processNextToken optimizedTokens {IlGenerator = ilGenerator; MemoryFI = memoryFI; PtrFI = ptrFI; LoopInfos = []}
        ilGenerator.Emit(OpCodes.Ret)