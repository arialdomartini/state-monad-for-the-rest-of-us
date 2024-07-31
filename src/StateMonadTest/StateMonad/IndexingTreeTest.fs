module StateMonadTest.StateMonad.IndexingTreeTest

open Xunit
open Swensen.Unquote
open StateMonadTest.StateMonad.StateMonad
open StateMonadTest.Tree

let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

let buildNode l r = Node(l, r)
let buildLeaf v count = Leaf(v, count)
let buildLeaf' v count _ = Leaf(v, count)
let run (State f) state = f state

let get = State(fun s -> (s, s))
let put s = State(fun _ -> ((), s))

let rec index =
    function
    | Leaf(v: string) ->
        state {
            let! count = get
            let leaf = Leaf(v, count)
            do! put (count + 1)
            return leaf
        }
    | Node(l, r) ->
        state {
            let! l = index l
            let! r = index r
            return (buildNode l r)
        }

[<Fact>]
let ``indexes a tree`` () =
    let withCount = index tree
    let indexed, _ = run withCount 1

    test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
