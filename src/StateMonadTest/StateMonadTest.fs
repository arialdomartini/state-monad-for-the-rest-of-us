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


type Tree<'a> =
    | Leaf of 'a
    | Node of Tree<'a> * Tree<'a>

// Tree a -> Int
let rec numberOfLeaves = function
    | Leaf _ -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r
    
// Tree String -> Tree Int    
let rec lengths = function
    | Leaf v -> Leaf (String.length v)
    | Node(l, r) -> Node (lengths l, lengths r)

[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let tree = Node(Leaf (), Node(Leaf (), Leaf ()))

    let leaves = numberOfLeaves tree

    test <@ leaves = 3 @>

[<Fact>]
let ``counts the leaves' content length`` () =
    let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

    let treeOfLengths = lengths treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>
