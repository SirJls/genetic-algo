open System

open Algorithm
open Utils

[<EntryPoint>]
let main argv =
    // All the buttons you can turn...
    let algo = Algorithm.New 0.8 0.3 100 32 10 true
    let problemDescription = "f(x) = -x^2 + 7x ="
    let fitnessFunc n = -pown n 2 + 7.0 * n
    algo.Run fitnessFunc problemDescription |> printIndividual
    0
