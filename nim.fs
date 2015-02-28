open System

type Player = Player1 | Player2

type Heap = HeapA | HeapB | HeapC

type GameState = {
    A : int
    B : int
    C : int
    Playing : Player
}

type Move = {
    heap : Heap
    take : int
}

let from available take =
    if available - take >= 0
    then available - take
    else 0

let Player1TakesPebbles gs move =
    match move with
    | { heap = HeapA; take = n } -> { gs with A = (from gs.A n); Playing = Player2 }
    | { heap = HeapB; take = n } -> { gs with B = (from gs.B n); Playing = Player2 }
    | { heap = HeapC; take = n } -> { gs with C = (from gs.C n); Playing = Player2 }

let Player2TakesPebbles gs move =
    match move with
    | { heap = HeapA; take = n } -> { gs with A = (from gs.A n); Playing = Player1 }
    | { heap = HeapB; take = n } -> { gs with B = (from gs.B n); Playing = Player1 }
    | { heap = HeapC; take = n } -> { gs with C = (from gs.C n); Playing = Player1 }

let rec PlayGame gs move = 
    match gs with
    | { Playing = Player1 } -> Player1TakesPebbles gs move
    | { Playing = Player2 } -> Player2TakesPebbles gs move

let getHeap (x:String) = 
    match (x.Split [|' '|]).[0] with
    | "A" -> HeapA
    | "B" -> HeapB
    | "C" -> HeapC
    | _ -> HeapA

let getStones (x:String) = 
    System.Int32.Parse((x.Split [|' '|]).[1])

let rec gameLoop gs =  

    printfn "A:%i B:%i C:%i" gs.A gs.B gs.C

    printfn "%A to go " gs.Playing
    
    let answer = Console.ReadLine()
    let heap = getHeap answer
    let stones = getStones answer
    let nextState = (PlayGame gs { heap = heap; take = stones; })

    match nextState with
    | { A = 0; B = 0; C = 0 } -> printfn "%A won" gs.Playing
    | _ -> gameLoop nextState

let mutable gs = { A = 5; B = 5; C = 5; Playing = Player1 }
gameLoop gs

let x = Console.ReadLine()

