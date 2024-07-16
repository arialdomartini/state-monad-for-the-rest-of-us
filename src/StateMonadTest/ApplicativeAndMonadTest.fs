module StateMonadTest.ApplicativeAndMonadTest

open Xunit
open Swensen.Unquote
open Tree

let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

//type WithCount<'v> = WithCount of (int -> Tree<'v * int> * int)
//type WithCount = WithCount of (int -> Tree<string * int> * int)
type WithCount<'v> = WithCount of (int -> 'v * int)
//type WithCount<'s, 'v> = WithCount of ('s -> 'v * 's)
//type WithCount<'f> = WithCount of 'f


let buildNode l r = Node(l, r)

// v -> Int -> Leaf (v, Int)
let buildLeaf v count = Leaf(v, count)

// v -> Int -> () -> Leaf (v, Int)
let buildLeaf' v count _ = Leaf(v, count)

let run (WithCount f) (count: int) = f count

let add x y = x + y

let (<|) f a = f a
let r = add <| 2 <| 3

let pure'<'v> (v: 'v) : WithCount<'v> = WithCount(fun count -> (v, count))

// WithCount (a -> b) -> WithCount a -> WithCount b
let (<*>) f a =
    WithCount(fun count ->
        let fv, fc = run f count
        let av, ac = run a fc
        let b = fv av
        b, ac)

let (<*) f v =
    WithCount(fun count ->
        let fv, fc = run f count
        let _, newCount = run v fc
        (fv, newCount))


let incCount = WithCount(fun c -> ((), c + 1))
let putCount c = WithCount(fun _ -> ((), c))
let incNum c = c + 1
let getCount = WithCount(fun c -> (c, c))

let (>>=) v f =
    WithCount(fun count ->
        let vv, cv = run v count
        let withCountB = f vv
        run withCountB cv)


type KeepState() =
    member this.Bind(v, f) = v >>= f
    member this.Return(v) = pure' v

let keepState = KeepState()

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
