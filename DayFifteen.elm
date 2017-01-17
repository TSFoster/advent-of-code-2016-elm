module DayFifteen exposing (answers)

import Model exposing (..)


answers : List QandA
answers =
    [ QandA (Question "Day 15 part 1") part1
    ]


type alias Disc =
    { stage : Int
    , numPositions : Int
    , startingPosition : Int
    }


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            nextTimeDiscsAlign input 0
                |> toString
        )


nextTimeDiscsAlign : List Disc -> Int -> Int
nextTimeDiscsAlign discs time =
    if discsWillAlign discs time then
        time
    else
        nextTimeDiscsAlign discs (time + 1)


discsWillAlign : List Disc -> Int -> Bool
discsWillAlign discs time =
    List.all (discWillAlign time) discs


discWillAlign : Int -> Disc -> Bool
discWillAlign time { stage, numPositions, startingPosition } =
    (time + stage + startingPosition) % numPositions == 0


input : List Disc
input =
    [ Disc 1 13 1
    , Disc 2 19 10
    , Disc 3 3 2
    , Disc 4 7 1
    , Disc 5 5 3
    , Disc 6 17 5
    ]
