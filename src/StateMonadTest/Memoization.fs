module StateMonadTest.Memoization

open System.Collections.Generic
open Xunit
open Swensen.Unquote

let memoized f =
    let cache = Dictionary<'a, 'b>()
    let cachedFunction =
        fun v ->
            if not (cache.ContainsKey v)
            then
                let result = f v
                cache.Add (v, result)
            cache[v]
    cachedFunction
        
    
[<Fact>]
let ``multiple invocations with the same arguments imply 2 executions`` () =
    let mutable numberOfExecutions = 0
    let invokeMe v =
        numberOfExecutions <- numberOfExecutions + 1
        v
    invokeMe 42 |> ignore
    invokeMe 42 |> ignore
    test <@ numberOfExecutions = 2 @>
    
[<Fact>]
let ``1 single execution if memoized`` () =
    let mutable numberOfExecutions = 0
    let invokeMe v =
        numberOfExecutions <- numberOfExecutions + 1
        v
        
    let memoized = memoized invokeMe
    
    memoized 42 |> ignore
    memoized 42 |> ignore
    test <@ numberOfExecutions = 1 @>
