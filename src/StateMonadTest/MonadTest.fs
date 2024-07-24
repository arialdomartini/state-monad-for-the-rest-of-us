module StateMonadTest.MonadTest

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

// (a -> WithCount b) -> WithCount a -> WithCount b
// let (=<<) f a  =
//     WithCount(fun count ->
//         let va, ca = run a count
//         let result = f va
//         run result ca)

// WithCount a -> (a -> WithCount b) -> WithCount b
let (>>=) a f =
    WithCount(fun count ->
        let va, ca = run a count
        let result = f va
        run result ca)

// (a -> WithCount b) -> WithCount a -> WithCount b
let (=<<) a b = b (>>=) a

let rec index =
    function
    | Leaf(v: string) ->
        getCount
        >>= (fun count ->
            let leaf = Leaf(v, count)
            putCount (count + 1)
            >>= (fun _ -> pure' leaf))
    | Node(l, r) ->
        index l >>= (fun ll ->
            index r
             >>= (fun rr ->
                 pure' (buildNode ll rr)))


[<Fact>]
let ``indexes a tree`` () =
    let withCount = index tree
    let indexed, _ = run withCount 1

    test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
