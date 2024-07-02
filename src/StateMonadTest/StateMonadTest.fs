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
let rec numberOfLeaves =
    function
    | Leaf _ -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r

// Tree String -> Tree Int
let rec lengths =
    function
    | Leaf v -> Leaf(String.length v)
    | Node(l, r) -> Node(lengths l, lengths r)

// (a -> b) -> Tree a -> Tree b
let rec map f tree =
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(map f l, map f r)

// (a -> b) -> Tree a -> Tree b
// let rec map f = function
//     | Leaf v -> Leaf (f v)
//     | Node(l, r) -> Node (map f l, map f r)


// Tree a -> (a -> b) -> Tree b
let rec map' tree f =
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(map' l f, map' r f)


[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let tree = Node(Leaf(), Node(Leaf(), Leaf()))

    let leaves = numberOfLeaves tree

    test <@ leaves = 3 @>

let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))


[<Fact>]
let ``counts the leaves' content length`` () =
    let treeOfLengths = lengths treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>

[<Fact>]
let ``counts the leaves' content length, using map'`` () =
    let treeOfLengths = map' treeOfWords String.length

    test <@ treeOfLengths = treeOfNumbers @>

[<Fact>]
let ``counts the leaves' content length, using map`` () =
    let mapped = map String.length
    let treeOfLengths = mapped treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>

let (^) = map

[<Fact>]
let ``counts the leaves' content length, lifting a function`` () =
    let treeOfLengths = String.length^ treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>
