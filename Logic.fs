module Logic

open System
open Individual

type Fitness = float

type Population = Individual[]

type Parents = Individual[]

type Children = Individual * Individual

type Fitnesses = Fitness[]

type IndividualWithFitness = Individual * float

type PopulationWithFitnesses = IndividualWithFitness[]

let randomDouble =
    let r = Random()
    fun () -> r.NextDouble()

let randomNextDouble a b =
    fun () -> b - (randomDouble() * (b - a))

let randomNextInt a b =
    fun () -> int ((randomNextDouble (float a) (float b))())

let createPopulation n maxValueIndividual : Population =
    let f _ = Individual.Create ((randomNextInt 0 maxValueIndividual)())
    Array.init n f

let computeFitness f (i : Individual) : IndividualWithFitness =
    let fitness = f (float i.Number) in i,fitness

let computeFitnesses f (p : Population) : PopulationWithFitnesses =
    Array.map (computeFitness f) p

let rouletteWheel (pop : PopulationWithFitnesses) =
    // To make sure there are no negatives add the opposite of the individual
    // with the smallest fitness to all individuals.
    let min = Array.minBy (fun (_,b) -> b) pop
    let pop' = Array.map (fun (a,b) -> a,(b + (abs (snd min)))) pop
    // After this operation we've effectively removed the chance for
    // selection the individual with the smallest fitness. To compensate,
    // we apply a small 'bias' to improve selection probability of this
    // individual.
    let bias = 0.01
    let sum = Array.sumBy (fun (_,b) -> (b + (b * bias))) pop'
    let random = randomDouble()
    let rec spin times cummulativeProbability =
        let (i,f) = pop'.[times]
        let cummulativeProbability' = cummulativeProbability + (f / sum)
        match cummulativeProbability',random,times with
            | c,r,_ when c >= r -> i
            | _,_,t when t = 0 -> fst <| pop'.[pop'.Length-1]
            | _ -> spin (times-1) cummulativeProbability'
    spin (pop.Length-1) 0.0

let selectParents n (a : PopulationWithFitnesses) : Parents =
    let parents = []
    let rec solution cycle p =
        if cycle = 0 then
            p
        else
            solution (cycle-1) ((rouletteWheel a) :: p)
    let parents' = solution n parents
    parents' |> List.toArray

let crossover (a : Individual) (b : Individual) : Children =
    let p1,p2 = if a.Number <= b.Number then a,b else b,a
    // two point crossover needs at least a number which is represented by
    // 3 bits
    if a.Number < 3 || b.Number < 3 then
        // single point crossover
        let half = (p1.Length 2) / 2
        let lowerSliceP1 = p1.GetBitRange half 0
        let lowerSliceP2 = p2.GetBitRange half 0
        let upperSliceP1 = p1.Number - lowerSliceP1
        let upperSliceP2 = p2.Number - lowerSliceP2
        let child1 = Individual <| upperSliceP1 + lowerSliceP2
        let child2 = Individual <| upperSliceP2 + lowerSliceP1
        child1,child2
    else
        // two point crossover
        let len = p1.Length 2
        let q1 = len / 3
        let q3 = len - (len / 3)
        let upperSliceP1 = p1.GetBitRange len q3
        let middleSliceP1 = p1.GetBitRange q3 q1
        let lowerSliceP1 = p1.GetBitRange q1 0
        let upperSliceP2 = p2.GetBitRange len q3
        let middleSliceP2 = p2.GetBitRange q3 q1
        let lowerSliceP2 = p2.GetBitRange q1 0
        let child1 = Individual <| lowerSliceP1 + middleSliceP2 + upperSliceP1
        let child2 = Individual <| lowerSliceP2 + middleSliceP1 + upperSliceP1
        child1,child2

let mutation (a : Individual) rate =
    if rate > randomDouble() then
        a.Mutate ((randomNextInt 0 (a.Length 2))())
    else
        a

