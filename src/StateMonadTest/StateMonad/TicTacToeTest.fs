module StateMonadTest.StateMonad.TicTacToeTest

open StateMonadTest.StateMonad.StateMonad
open StateMonadTest.StateMonad.RandomTest.LinearCongruentialGenerator
open Xunit
open Swensen.Unquote

type Player = XPlayer | OPlayer
type TileState = Empty | HasX | HasO
type Index = int * int
type Tile = Index * TileState
type Board =  Tile list
type Seed = int64

type GameState = {
    Board: Board
    CurrentPlayer: Player
    RandomNumberGenerator: Seed
}

let randomNumberFrom0ton (n: int) = state {
    let! bigNumber = nextInt
    let upToN = int (bigNumber % (int64 n))
    return upToN
    }

let rndUpTo =
    fun (n: int) ->
        State (fun gameState ->
            let v, s = run (randomNumberFrom0ton n) gameState.RandomNumberGenerator
            (v, {gameState with RandomNumberGenerator =  s})) 

let findAvailableTiles (board: Board) : Tile list=
    board
    |> List.where (fun (_, state) -> state = Empty)

let nextPlayer player =
    match player with
    | XPlayer -> OPlayer
    | OPlayer -> XPlayer

let stateForPlayer (player: Player) =
    match player with
    | XPlayer -> HasX
    | OPlayer -> HasO

let update (board: Board) tile (player: Player) =
    let index,_ = tile 
    board |> List.map (fun (i, state) -> if i = index then (i, stateForPlayer player) else (i, state))

let randomAvailableTile : State<GameState, Tile> =
    state {
        let! gameState = get
        let availableTiles = gameState.Board |> findAvailableTiles
        let! randomIndex = rndUpTo (List.length availableTiles)
        let randomTile = availableTiles[randomIndex]
        return randomTile
    }

let doRandomMove : State<GameState, GameState> =
    state {
        let! randomTile = randomAvailableTile
        let! gameState = get        
        let newGameState = {gameState with
                             Board = update gameState.Board randomTile gameState.CurrentPlayer
                             CurrentPlayer = nextPlayer gameState.CurrentPlayer
                             }
            
        return newGameState
    }


let isCompleted (gameState: GameState) =
    gameState.Board |> findAvailableTiles = []

let rec play : State<GameState, GameState> =
    state {
        let! gameState = doRandomMove
        do! put gameState
        if gameState |> isCompleted then
            return gameState
        else
            return! play
    }

let anEmptyBoard = [
    ((1,1), Empty)
    ((1,2), Empty)
    ((1,3), Empty)
    
    ((2,1), Empty)
    ((2,2), Empty)
    ((2,3), Empty)
    
    ((3,1), Empty)
    ((3,2), Empty)
    ((3,3), Empty)
]

[<Fact>]
let ``tic tac toe`` () =
    let emptyBoard = { GameState.Board = anEmptyBoard ; CurrentPlayer = XPlayer; RandomNumberGenerator = 1 }
    let finalState, _ = run play emptyBoard
    
    test <@ finalState.Board |> List.forall (fun (_, s) -> s <> Empty) = true @>
