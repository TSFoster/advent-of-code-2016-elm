module DayNineteen exposing (answers)

import Model exposing (..)


answers : List QandA
answers =
    [ QandA (Question "Day 19 part 1") part1
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            toString <| elfWins puzzleInput
        )


elfWins : Int -> Int
elfWins i =
    2 * (i - 2 ^ (floor (logBase 2 (toFloat i)))) + 1


puzzleInput : Int
puzzleInput =
    3005290
