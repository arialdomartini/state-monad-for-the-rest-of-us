module StateMonadTest.Tree

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
