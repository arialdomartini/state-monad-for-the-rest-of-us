module StateMonadTest.StateMonad.StackTest

open Xunit
open Swensen.Unquote
open StateMonadTest.StateMonad.StateMonad

type Stack<'s, 'a> = State<'s list, 'a>



let pop =
    state {
        let! stack = get
        let h :: t = stack
        do! put t
        return h
    }


let push (v: 'v) : State<'v list, unit> =
    state {
        let! stack = get
        let newStack = v :: stack
        do! put newStack
        return ()
    }

let toInteger (str: string) = System.Int32.TryParse str

let asInteger (str: string) : int =
    let _, i = System.Int32.TryParse str
    i

let isInteger (str: string) : bool =
    let is, _ = System.Int32.TryParse str
    is

let ops = dict [ ("+", (+)); ("*", (*)); ("-", (-)) ]


[<Fact>]
let ``uses a LIFO stack`` () =
    let eval input =
        if input |> isInteger then
            state {
                do! push input
                return input
            }
        else
            let op = ops[input]

            state {
                let! a = pop
                let! b = pop
                let result = op (b |> asInteger) (a |> asInteger)
                do! push (result.ToString())
                return result.ToString()
            }

    let expected = ((2 + 3) * (5 * (50 - 40))).ToString()
    let inputs = [ "2"; "3"; "+"; "5"; "50"; "40"; "-"; "*"; "*" ]

    let r1 =
        eval "2"
        >> eval "3"
        >> eval "+"
        >> eval "5"
        >> eval "50"
        >> eval "40"
        >> eval "-"
        >> eval "*"
        >> eval "*"

    let r2 = List.foldBack (fun s x -> eval s >> x) inputs pop 
    let r3 =
        let mapped = inputs |> List.map eval
        List.foldBack (fun s x -> s >> x) mapped pop
    
    let result1, _ = run r1 []
    let result2, _ = run r2 []
    let result3, _ = run r3 []
    
    test <@ result1 = expected @>
    test <@ result2 = expected @>
    test <@ result3 = expected @>
     

// Alternative implementations
let push' v =
     State (fun s -> ((), v::s))

let pop' =
    State (fun s -> (List.head s, List.tail s))
    
[<Fact>]
let ``alternative implementations`` () =
    let eval input =
        if input |> isInteger then
            state {
                do! push' input
                return input
            }
        else
            let op = ops[input]

            state {
                let! a = pop'
                let! b = pop'
                let result = op (b |> asInteger) (a |> asInteger)
                do! push' (result.ToString())
                return result.ToString()
            }

    let expected = ((2 + 3) * (5 * (50 - 40))).ToString()

    let r1 =
        eval "2"
        >> eval "3"
        >> eval "+"
        >> eval "5"
        >> eval "50"
        >> eval "40"
        >> eval "-"
        >> eval "*"
        >> eval "*"

    let result1, _ = run r1 []
    
    test <@ result1 = expected @>
