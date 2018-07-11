module Algorithm

open System
open Individual
open Logic

type Algorithm =
    {
        crossoverRate : float
        mutationRate : float
        populationCount : int
        // This is exclusive e.g.:
        // let x be maxValueIndividual => x - 1
        maxValueIndividual : int
        generations : int
        elitism : bool
    }
    static member Default =
        { crossoverRate = 0.8; mutationRate = 0.3; populationCount = 10; maxValueIndividual = 32; generations = 100; elitism = true; }
    static member New c m p v g e =
        { crossoverRate = c; mutationRate = m; populationCount = p; maxValueIndividual = v; generations = g; elitism = e; }
    member m.Run (f0 : int -> int -> Population)
                 (f1 : (float -> float) -> Population -> PopulationWithFitnesses)
                 (f2 : int -> PopulationWithFitnesses -> Parents)
                 (f3 : Individual -> Individual -> Children)
                 (f4 : Individual -> float -> Individual) =
                     let fitnessFunc n = -pown n 2 + 7.0 * n
                     let probabilityCrossover (parents : Parents) : Children =
                         if (Logic.randomDouble() < m.crossoverRate) then
                             f3 parents.[0] parents.[1]
                         else
                             parents.[0],parents.[1]
                     let maybePreserveStrongest (p : PopulationWithFitnesses) =
                         // Preserve the strongest individual
                         if m.elitism then
                             [fst (Array.maxBy snd p)]
                         else
                             []

                     let initialPopulation = f0 m.populationCount m.maxValueIndividual

                     let rec evolve generation population =
                         let trainedPopulation = f1 fitnessFunc initialPopulation

                         let rec nextPopulation current =
                             let parents = f2 2 trainedPopulation
                             let children = probabilityCrossover parents
                             let child1 = fst children
                             let child2 = snd children
                             let current' = (f4 child1 m.mutationRate) :: current
                             if (current'.Length < m.populationCount) then
                                 // Room to add the second child to the population
                                 let current'' = (f4 child2 m.mutationRate) :: current'
                                 if (current''.Length = m.populationCount) then
                                     // Added second child, if the population is full after this operation return the population
                                     current''
                                 else
                                     nextPopulation current''
                             else
                                 current'

                         let currentPopulation =
                             nextPopulation (maybePreserveStrongest trainedPopulation) |> List.toArray

                         match generation with
                             | g when g = 0 -> currentPopulation
                             | _ -> evolve (generation - 1) currentPopulation

                     let finalPopulation = evolve m.generations initialPopulation

                     // Recompute the fitness for every individual in the final
                     /// population and return the best individual

                     // To get the answer to the maximisation problem, we need
                     // to get the average of the equally strong individuals
                     let final = f1 fitnessFunc finalPopulation
                     let best = (Array.maxBy snd final)
                     let equalToBest = Array.FindAll (final,(fun (_,b) -> (snd best) = b))
                     let solution = Array.averageBy (fun (a : Individual,b) -> float (a.Number)) equalToBest
                     let problemDescription = "f(x) = -x^2 + 7x =" 
                     problemDescription,solution,best |> fst
