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


let build l r = Node(l, r)
let run (WithCount f) (count: int)= f count
 
let add x y = x + y

let (<|) f a = f a 
let r = add <| 2 <| 3

let pure'<'v> (v: 'v) : WithCount<'v> = WithCount (fun count -> (v, count))

// WithCount (a -> b) -> WithCount a -> WithCount b 
let (<*>) f a = 
    WithCount (fun count ->
        let fv, fc = run f count
        let av, ac = run a fc
        let b = fv av
        b, ac)

let incCount = WithCount (fun c -> (), c + 1)
let putCount c = WithCount (fun _ -> (), c)
let getCount = WithCount (fun c -> c, c)
//
// let incCount =
//     let f get put =
//         let count = get
//         let _ = put (count + 1)
//         ()
//     pure' f <*> getCount <*> putCount

let buildLeaf v count put =
    let leaf = Leaf (v, count)
    put (count + 1) |> ignore
    leaf

let rec index<'a> =
    function
    | Leaf v ->
        // pure' buildLeaf <*> (pure' v) <*> getCount <*> (pure' putCount)
        pure' buildLeaf <*> (pure' v) <*> getCount <*> pure' putCount
    | Node(l, r) ->
        pure' build <*> index l <*> index r

[<Fact>]
let ``indexes a tree`` () =
    let withCount = index tree
    let indexed, _ = run withCount 1

    test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
