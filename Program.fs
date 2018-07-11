open System

open Algorithm

[<EntryPoint>]
let main argv =
    // All the buttons you can turn...
    let algo = Algorithm.New 0.8 0.3 100 32 10 true
    algo.Run
        Logic.createPopulation
        Logic.computeFitnesses
        Logic.selectParents
        Logic.crossover
        Logic.mutation |> Logic.printIndividual
    0
