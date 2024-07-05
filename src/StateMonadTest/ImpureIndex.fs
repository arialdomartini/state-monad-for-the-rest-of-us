module StateMonadTest.ImpureIndexTest

open Xunit
open Swensen.Unquote
open Tree

let rec map f =
    function
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(map f l, map f r)

let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

[<Fact>]
let ``indexes a tree`` () =
    let mutable counter = 1
    let impureIndex v =
        let indexedLeaf = (v, counter)
        counter <- counter + 1
        indexedLeaf
    
    let indexed = map impureIndex tree

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>

let invokedTwice f =
    fun v ->
        f v |> ignore
        f v

[<Fact>]
let ``does not work anymore if executed twice`` () =
    let mutable counter = 1
    let impureIndex v =
        let indexedLeaf = (v, counter)
        counter <- counter + 1
        indexedLeaf
    
    let indexed = map (impureIndex |> invokedTwice) tree

    test <@ indexed <> Node(Leaf ("one", 1), Node(Leaf ("two", 3), Leaf ("three", 3))) @>

[<Fact>]
let ``does not work anymore if executed on 2 different trees`` () =
    let mutable counter = 1
    let impureIndex v =
        let indexedLeaf = (v, counter)
        counter <- counter + 1
        indexedLeaf
    
    let indexed1 = map impureIndex tree
    let indexed2 = map impureIndex tree

    test <@ indexed2 <> Node(Leaf ("one", 1), Node(Leaf ("two", 3), Leaf ("three", 3))) @>

let rec index tree =
    match tree with
    | Leaf v -> fun c -> (Leaf (v, c), c+1)
    | Node (l, r) ->
        fun c -> 
            let vl, cl = index l c
            let vr, cr = index r cl
            (Node (vl, vr), cr)

[<Fact>]
let ``pure implementation`` () =

    let indexed, _ = (index tree) 1

    test <@ indexed <> Node(Leaf ("one", 1), Node(Leaf ("two", 3), Leaf ("three", 3))) @>
