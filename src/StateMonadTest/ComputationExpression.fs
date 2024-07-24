module StateMonadTest.ComputationExpression

open Xunit
open Swensen.Unquote
open Tree

let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

type WithCount<'v> = WithCount of (int -> 'v * int)

// Tree v -> Tree v -> Tree v
let buildNode l r = Node(l, r)

// v -> Int -> Leaf (v, Int)
let buildLeaf v count = Leaf(v, count)

// v -> Int -> () -> Leaf (v, Int)
let buildLeaf' v count _ = Leaf(v, count)

// WithCount v -> Int -> v
let run (WithCount f) (count: int) = f count

// v -> WithCount v
let pure' v = WithCount(fun count -> (v, count))

// WithCount (Int, Int)
let getCount = WithCount(fun c -> (c, c))

// Int -> WithCount ((), Int)
let putCount c = WithCount(fun _ -> ((), c))

// WithCount a -> (a -> WithCount b) -> WithCount b
let (>>=) a f =
    WithCount(fun count ->
        let va, ca = run a count
        let result = f va
        run result ca)

type WithCountExpression() =
    member this.Return(v) = pure' v 
    member this.Bind(v, f) = v >>= f 
    
let withCount = WithCountExpression()

let rec index =
    function
    | Leaf v ->
        withCount {
            let! count = getCount
            let leaf = Leaf (v, count)
            do! putCount (count + 1)
            return leaf
        }
    | Node(l, r) ->
        withCount {
            let! ll = index l
            let! rr = index r
            return buildNode ll rr
        }

[<Fact>]
let ``indexes a tree`` () =
    let withCount = index tree
    let indexed, _ = run withCount 1

    test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>


let x : WithCount<string> =
    // withCount {
    //   do! putCount 42
    //   do! putCount 0
    //   do! putCount 99
    //   return "Hello, world!"
    // }
    putCount 42 >>= (fun _ ->
      putCount 0 >>= (fun _ ->
          putCount 99 >>= (fun _ ->
              pure' "Hello, world!")))
