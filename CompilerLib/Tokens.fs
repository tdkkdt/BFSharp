namespace CompilerLib

type Token = IncrementPtr
            | DecrementPtr
            | IncrementData
            | DecrementData
            | OutputData
            | InputData
            | LoopStart
            | LoopEnd
            | Unknown

module Tokens = 
    let tokenize (code : string): Token list = 
        code.ToCharArray() |> Array.toList |> List.map (fun x -> 
            match x: char with
                | '>' -> IncrementPtr
                | '<' -> DecrementPtr
                | '+' -> IncrementData
                | '-' -> DecrementData
                | '.' -> OutputData
                | ',' -> InputData
                | '[' -> LoopStart
                | ']' -> LoopEnd
                | _ -> Unknown
        ) |> List.filter ((<>) Token.Unknown)