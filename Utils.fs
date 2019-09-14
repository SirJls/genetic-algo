module Utils

open Individual

let printIndividual (a : string * float * Individual) =
    let (d,s,i) = a
    printfn "The heuristic solution to %s %.2f" d s
    printfn "The final individual: %A" i
    printfn "Integer representation: %d" i.Number
    printfn "Binary representation: %s" i.Binary
    printfn "Array representation: %A" i.BinaryArray
    printfn "Length in base 16: %d" (i.Length 16)
    printfn "Length in base 10: %d" (i.Length 10)
    printfn "Length in base 8: %d" (i.Length 8)
    printfn "Length in base 2: %d" (i.Length 2)
