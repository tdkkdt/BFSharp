namespace CompilerLib

type SyntaxCheckResult = OK | CodeEmpty | LoopsProblem

type SyntaxChecker() = 
    member this.CheckSyntax (tokens : Token list): SyntaxCheckResult = 
        if tokens.IsEmpty then
            CodeEmpty
        else
            let bracketsCounter previouslyOpenedBrackets currentToken = 
                match previouslyOpenedBrackets with
                    | -1 -> previouslyOpenedBrackets
                    | _ when currentToken = LoopStart -> previouslyOpenedBrackets + 1
                    | _ when currentToken = LoopEnd -> previouslyOpenedBrackets - 1
                    | _ -> previouslyOpenedBrackets
            let bracketsCount = tokens |> List.fold bracketsCounter 0
            if bracketsCount <> 0 then LoopsProblem else OK