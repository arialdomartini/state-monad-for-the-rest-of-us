module StateMonadTest.ApplicativeTest

open Xunit
open Swensen.Unquote
open Tree

let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

type WithCount<'v> = WithCount of (int -> 'v * int)
//type WithCount<'v> = WithCount of (int -> Tree<'v * int> * int)
//type WithCount = WithCount of (int -> Tree<string * int> * int)
//type WithCount<'s, 'v> = WithCount of ('s -> 'v * 's)
//type WithCount<'f> = WithCount of 'f

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

// WithCount (a -> b) -> WithCount a -> WithCount b
let (<*>) f a =
    WithCount(fun count ->
        let fv, fc = run f count
        let av, ac = run a fc
        let b = fv av
        b, ac)

// WithCount a -> WithCount b -> WithCount a
let (<*) f v =
    WithCount(fun count ->
        let fv, fc = run f count
        let _, newCount = run v fc
        (fv, newCount))


let incCount = WithCount(fun c -> ((), c + 1))
let putCount c = WithCount(fun _ -> ((), c))
let incNum c = c + 1
let getCount = WithCount(fun c -> (c, c))


let incrementCount: WithCount<unit> = WithCount(fun count -> (), count + 1)

let rec index<'a> =
    function
    | Leaf v -> pure' buildLeaf <*> pure' v <*> getCount <* incrementCount
    | Node(l, r) -> pure' buildNode <*> index l <*> index r

let rec index'<'a> =
    function
    | Leaf v -> pure' buildLeaf' <*> pure' v <*> getCount <*> incrementCount
    | Node(l, r) -> pure' buildNode <*> index l <*> index r

[<Fact>]
let ``indexes a tree`` () =
    let withCount = index tree
    let indexed, _ = run withCount 1

    test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>

[<Fact>]
let ``indexes a tree using buildLeaf with extra parameter`` () =
    let withCount = index' tree
    let indexed, _ = run withCount 1

    test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
