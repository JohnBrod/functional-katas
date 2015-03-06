open System

type Player = Player1 | Player2

type GameState = {
    A : int
    B : int
    C : int
    Playing : Player
}

type Move = {
    heap : String
    take : int
}

let take available take =
    if available - take >= 0
    then available - take
    else 0

let next player = 
    if player = Player1 then Player2 else Player1

let PlayerTakesPebbles gs move =
    match move with
    | { heap = "A"; take = n } -> { gs with A = (take gs.A n); Playing = next gs.Playing }
    | { heap = "B"; take = n } -> { gs with B = (take gs.B n); Playing = next gs.Playing }
    | { heap = "C"; take = n } -> { gs with C = (take gs.C n); Playing = next gs.Playing }
    | _ -> gs

let getAnswer (x:String) = 
    let items = x.Split [|' '|]
    (items.[0], System.Int32.Parse(items.[1]))

let rec gameLoop gs =  

    printfn "A:%i B:%i C:%i" gs.A gs.B gs.C

    printfn "%A to go " gs.Playing
    
    let heap, stones = getAnswer (Console.ReadLine())
    let nextState = (PlayerTakesPebbles gs { heap = heap; take = stones; })

    match nextState with
    | { A = 0; B = 0; C = 0 } -> printfn "%A won" gs.Playing
    | _ -> gameLoop nextState

let gs = { A = 5; B = 5; C = 5; Playing = Player1 }
gameLoop gs

let x = Console.ReadLine()

