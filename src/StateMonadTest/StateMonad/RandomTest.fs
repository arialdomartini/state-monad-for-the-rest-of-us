module StateMonadTest.StateMonad.RandomTest

open StateMonadTest.StateMonad.StateMonad
open Xunit
open Swensen.Unquote


module LinearCongruentialGenerator =
    type LCGState = int64

    // Parameters for the LCG
    let a: int64 = 1103515245L
    let c: int64 = 12345L
    let m: int64 = 2 <<< 31

    let next (state: LCGState) : int * LCGState =
        let nextState = (a * state + c) % m
        let randomInt = int (nextState >>> 16)  // Use upper bits for better randomness
        (randomInt, nextState)

    let nextInt : State<int64, int64> =
        state {
            let! prev = get
            let nextState = (a * prev + c) % m
            let randomInt = int (nextState >>> 16)  // Use upper bits for better randomness
            return randomInt
        }

let rec nextIntList count = state {
        if count = 0 then 
            return []
        else
            let! head = LinearCongruentialGenerator.nextInt
            let! tail = (nextIntList (count - 1))
            return head :: tail
    }

[<Fact>]
let ``each of the 100 seeds generate a unique pseudo-random numbers collection`` () =
    let sets =
        [1..100]
        |> List.map (fun i ->
            let random, _ = run (nextIntList 50) i in random)
        
    let setsWithoutDuplicates = sets |> List.map (fun s -> (s |> List.sort)) |> List.distinct
    
    test <@ setsWithoutDuplicates |> List.length = (sets |> List.length) @>
