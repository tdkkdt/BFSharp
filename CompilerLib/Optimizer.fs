namespace CompilerLib

module Optimizer =
    let rec private tryToOptimizePointerOperation (optimized : Token list) (value : int) (tokens : Token list) : Token list =
        let appendToOptimzed = fun () -> if value = 0 then optimized else MovePtrDelta(value)::optimized
        match tokens with
            | [] -> List.rev (appendToOptimzed())
            | h::t -> match h with
                            | IncrementPtr -> tryToOptimizePointerOperation optimized (value + 1) t
                            | DecrementPtr -> tryToOptimizePointerOperation optimized (value - 1) t
                            | MovePtrDelta(delta) -> tryToOptimizePointerOperation optimized (value + delta) t
                            | _ -> tryToOptimizePointerOperation (h::appendToOptimzed()) 0 t
  
    let rec private tryToOptimizeDataOperation (optimized : Token list) (value : int) (tokens : Token list) : Token list =
        let appendToOptimzed = fun () -> if value = 0 then optimized else SetDataDelta(value)::optimized
        match tokens with
            | [] -> List.rev (appendToOptimzed())
            | h::t -> match h with
                            | IncrementData -> tryToOptimizeDataOperation optimized (value + 1) t
                            | DecrementData -> tryToOptimizeDataOperation optimized (value - 1) t
                            | SetDataDelta(delta) -> tryToOptimizeDataOperation optimized (value + delta) t
                            | _ -> tryToOptimizeDataOperation (h::appendToOptimzed()) 0 t

    let rec private tryToOptimizeClearDataOperation (optimized : Token list) (tokens : Token list) : Token list =
        match tokens with 
            | [] -> List.rev optimized
            | h::t -> match h with
                            | LoopStart when t.IsEmpty = false && (t.Head = SetDataDelta(1) || t.Head = SetDataDelta(-1)) && t.Tail.IsEmpty = false && t.Tail.Head = LoopEnd -> 
                                tryToOptimizeClearDataOperation (ClearData::optimized) t.Tail.Tail
                            | _ -> tryToOptimizeClearDataOperation (h::optimized) t

    let rec private doFullOptimizations (tokens : Token list) : Token list =
        let result = tokens |>
                        tryToOptimizePointerOperation [] 0 |>
                        tryToOptimizeDataOperation [] 0 |>
                        tryToOptimizeClearDataOperation []
        if result.Length = tokens.Length then result else doFullOptimizations result
    
    let optimizeTokens (tokens : Token list) : Token list =
        doFullOptimizations tokens