// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open CompilerLib

[<EntryPoint>]
let main argv = 
    let bfFileName = argv.[0]
    let exeFileName = argv.[1]
    Compiler.compileFromFile bfFileName exeFileName
    printf "Done."
    0