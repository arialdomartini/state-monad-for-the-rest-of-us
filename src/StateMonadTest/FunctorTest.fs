module StateMonadTest.FunctorTest

open Xunit
open Swensen.Unquote
open StateMonadTest.Tree

let baseCase _ = 1
let baseCase' v = Leaf(String.length v)
let (^+) l r = Node (l, r)

// Tree a -> Int
let rec numberOfLeaves =
    function
    | Leaf v -> baseCase v
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r


// Tree String -> Tree Int
let rec lengths =
    function
    | Leaf v -> baseCase' v
    | Node(l, r) -> lengths l ^+ lengths r

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
let rec transform tree f =
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(transform l f, transform r f)


[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let treeWith3Leaves = Node(Leaf(), Node(Leaf(), Leaf()))

    let leaves = numberOfLeaves treeWith3Leaves

    test <@ leaves = 3 @>


let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))


[<Fact>]
let ``calculate the leaves' content length`` () =
    let treeOfLengths = lengths treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>

[<Fact>]
let ``calculate the leaves' content length, using transform`` () =
    let treeOfLengths = transform treeOfWords String.length

    test <@ treeOfLengths = treeOfNumbers @>


[<Fact>]
let ``calculate the leaves' content length, using map`` () =
    let mapped: Tree<string> -> Tree<int> = map String.length
    let treeOfLengths = mapped treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>

let (^) = map

[<Fact>]
let ``calculate the leaves' content length, lifting a function`` () =
    let treeOfLengths = String.length ^ treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>
