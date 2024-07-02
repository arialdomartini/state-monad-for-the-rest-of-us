module StateMonadTest

open Xunit
open Swensen.Unquote

//     Node
//     /  \
//  Leaf  Leaf

//     Node
//     /  \
//  Leaf  Node
//        /  \
//    Leaf   Leaf

//       Leaf


type Tree =
    | Leaf
    | Node of Tree * Tree

let rec numberOfLeaves = function
    | Leaf -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r

[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let tree = Node(Leaf, Node(Leaf, Leaf))

    let leaves = numberOfLeaves tree

    test <@ leaves = 3 @>
