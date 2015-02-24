module main

module NimDomain =

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
        numberToTake : int
    }

    let Player1TakesPebbles gs move =
        match move with
        | { heap = HeapA; numberToTake = n } -> { gs with A = gs.A - n; Playing = Player2 }
        | { heap = HeapB; numberToTake = n } -> { gs with B = gs.B - n; Playing = Player2 }
        | { heap = HeapC; numberToTake = n } -> { gs with C = gs.C - n; Playing = Player2 }

    let Player2TakesPebbles gs move =
        match move with
        | { heap = HeapA; numberToTake = n } -> { gs with A = gs.A - n; Playing = Player1 }
        | { heap = HeapB; numberToTake = n } -> { gs with B = gs.B - n; Playing = Player1 }
        | { heap = HeapC; numberToTake = n } -> { gs with C = gs.C - n; Playing = Player1 }

    let rec PlayGame gs move = 
        match gs with
        | { A = 0; B = 0;} -> gs
        | { Playing = Player1 } -> PlayGame (Player1TakesPebbles gs move) move
        | { Playing = Player2 } -> PlayGame (Player2TakesPebbles gs move) move

    let mutable gs = { A = 5; B = 5; C = 5; Playing = Player1 }
    gs <- PlayGame gs { heap = HeapA; numberToTake = 1; }

    printfn "%A won" gs.Playing


