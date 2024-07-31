module StateMonadTest.StateMonad.StateMonad

type State<'s, 'v> = State of ('s -> 'v * 's)

let run (State f) state = f state

let pure' v = State(fun s -> (v, s))

let get = State(fun s -> (s, s))
let put s = State(fun _ -> ((), s))

let (>>=) v f =
    State(fun s ->
        let va, sa = run v s
        let result = f va
        run result sa)

let (=<<) a b = b (>>=) a

let (>>) a b = a >>= (fun _ -> b)
    


type StateExpression() =
    member this.Return(v) = pure' v 
    member this.Bind(v, f) = v >>= f
    
let state = StateExpression()
