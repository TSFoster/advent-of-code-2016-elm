module DayThirteen exposing (answers)

import Model exposing (..)
import Bitwise
import Set exposing (Set)
import AStar.Generalised exposing (findPath)


answers : List QandA
answers =
    [ QandA (Question "Day 13 part 1") part1
    , QandA (Question "Day 13 part 2") part2
    ]


type alias Position =
    ( Int, Int )


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            findPath costFn movesFrom ( 1, 1 ) ( 31, 39 )
                |> Maybe.map (List.length >> toString)
                |> Maybe.withDefault "No solution found"
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            positionsAfterMaxSteps 50 (Set.singleton ( 1, 1 ))
                |> Set.size
                |> toString
        )


positionsAfterMaxSteps : Int -> Set Position -> Set Position
positionsAfterMaxSteps i steps =
    if i < 1 then
        steps
    else
        let
            newSteps =
                steps
                    |> Set.toList
                    |> List.map movesFrom
                    |> List.foldl Set.union steps
        in
            positionsAfterMaxSteps (i - 1) newSteps


costFn : Position -> Position -> Float
costFn ( x1, y1 ) ( x2, y2 ) =
    toFloat <| abs (x2 - x1) + abs (y2 - y1)


movesFrom : Position -> Set Position
movesFrom ( x, y ) =
    [ ( x, y + 1 )
    , ( x, y - 1 )
    , ( x + 1, y )
    , ( x - 1, y )
    ]
        |> List.filter isOpenSpace
        |> Set.fromList


isOpenSpace : Position -> Bool
isOpenSpace ( x, y ) =
    if x < 0 || y < 0 then
        False
    else
        countOfBinaryOnesIsEven (x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber)


countOfBinaryOnesIsEven : Int -> Bool
countOfBinaryOnesIsEven =
    let
        isEven : Bool -> Int -> Bool
        isEven bool n =
            if n == 0 then
                bool
            else
                let
                    newN =
                        Bitwise.shiftRightZfBy 1 n

                    newBool =
                        if n % 2 == 1 then
                            not bool
                        else
                            bool
                in
                    isEven newBool newN
    in
        isEven True


favoriteNumber : Int
favoriteNumber =
    1352
