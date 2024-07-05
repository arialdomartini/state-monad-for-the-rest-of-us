module StateMonadTest.ApplicativeAndMonadTest

open Xunit
open Swensen.Unquote
open Tree

let mutable count = 1
let rec imperativeIndex =
    function
    | Leaf v ->
        let indexedLeaf = Leaf (v, count)
        count <- count + 1
        indexedLeaf
    | Node(l, r) -> Node(imperativeIndex l, imperativeIndex r)

let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))


[<Fact>]
let ``impurely indexes a tree`` () =
    let indexed = imperativeIndex tree

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>

// Tree a -> (Int -> (Tree (a, Int), Int))
let rec index =
    function
    | Leaf v -> fun count -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        fun count ->
            let li, lc = index l count
            let ri, rc = index r lc
            Node (li, ri), rc

[<Fact>]
let ``indexes a tree`` () =
    let indexed, _ = index tree 1

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
