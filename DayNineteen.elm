module DayNineteen exposing (answers)

import Model exposing (..)


answers : List QandA
answers =
    [ QandA (Question "Day 19 part 1") part1
    , QandA (Question "Day 19 part 2") part2
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            toString <| elfWins puzzleInput
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            toString <| elfWinsSecond puzzleInput
        )


elfWins : Int -> Int
elfWins i =
    2 * (i - 2 ^ (floor (logBase 2 (toFloat i)))) + 1


elfWinsSecond : Int -> Int
elfWinsSecond n =
    let
        lastPowerOfThree =
            3 ^ (floor (logBase 3 (toFloat n - 0.5)))
    in
        min lastPowerOfThree (n - lastPowerOfThree) + max 0 (2 * (n - 2 * lastPowerOfThree))


puzzleInput : Int
puzzleInput =
    3005290
