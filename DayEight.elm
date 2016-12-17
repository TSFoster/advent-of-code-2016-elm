module DayEight exposing (answers)

import Model exposing (..)


answers : List QandA
answers =
    [ QandA (Question "Day 8 part 1") part1
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            List.foldl processInstruction (init 50 6) input
                |> List.map (List.filter identity)
                |> List.concat
                |> List.length
                |> toString
        )


type alias Screen =
    List (List Bool)


init : Int -> Int -> Screen
init w h =
    List.repeat w (List.repeat h False)


type Instruction
    = Rect Int Int
    | RotateRow Int Int
    | RotateColumn Int Int


processInstruction : Instruction -> Screen -> Screen
processInstruction instruction screen =
    case instruction of
        Rect w h ->
            List.take w screen
                |> List.map (List.drop h)
                |> List.map ((++) (List.repeat h True))
                |> (flip (++)) (List.drop w screen)

        RotateRow x y ->
            transpose screen
                |> processInstruction (RotateColumn x y)
                |> transpose

        RotateColumn x y ->
            let
                before =
                    List.take x screen

                col =
                    List.drop x screen |> List.take 1

                after =
                    List.drop (x + 1) screen
            in
                before ++ (List.map (rotate y) col) ++ after


transpose : List (List a) -> List (List a)
transpose grid =
    let
        heads =
            List.filterMap List.head grid

        tails =
            List.filterMap List.tail grid
    in
        if heads == [] then
            []
        else
            heads :: (transpose tails)


rotate : Int -> List a -> List a
rotate by list =
    let
        len =
            List.length list

        amount =
            len - (by % len)
    in
        List.drop amount list ++ List.take amount list


input : List Instruction
input =
    [ Rect 1 1
    , RotateRow 0 20
    , Rect 1 1
    , RotateRow 0 2
    , Rect 1 1
    , RotateRow 0 3
    , Rect 2 1
    , RotateRow 0 2
    , Rect 1 1
    , RotateRow 0 3
    , Rect 2 1
    , RotateRow 0 2
    , Rect 1 1
    , RotateRow 0 4
    , Rect 2 1
    , RotateRow 0 2
    , Rect 1 1
    , RotateRow 0 2
    , Rect 1 1
    , RotateRow 0 2
    , Rect 1 1
    , RotateRow 0 3
    , Rect 2 1
    , RotateRow 0 2
    , Rect 1 1
    , RotateRow 0 5
    , Rect 1 1
    , RotateRow 0 2
    , Rect 1 1
    , RotateRow 0 6
    , Rect 5 1
    , RotateRow 0 2
    , Rect 1 3
    , RotateRow 2 8
    , RotateRow 0 8
    , RotateColumn 0 1
    , Rect 7 1
    , RotateRow 2 24
    , RotateRow 0 20
    , RotateColumn 5 1
    , RotateColumn 4 2
    , RotateColumn 2 2
    , RotateColumn 0 1
    , Rect 7 1
    , RotateColumn 34 2
    , RotateColumn 22 1
    , RotateColumn 15 1
    , RotateRow 2 18
    , RotateRow 0 12
    , RotateColumn 8 2
    , RotateColumn 7 1
    , RotateColumn 5 2
    , RotateColumn 2 1
    , RotateColumn 0 1
    , Rect 9 1
    , RotateRow 3 28
    , RotateRow 1 28
    , RotateRow 0 20
    , RotateColumn 18 1
    , RotateColumn 15 1
    , RotateColumn 14 1
    , RotateColumn 13 1
    , RotateColumn 12 2
    , RotateColumn 10 3
    , RotateColumn 8 1
    , RotateColumn 7 2
    , RotateColumn 6 1
    , RotateColumn 5 1
    , RotateColumn 3 1
    , RotateColumn 2 2
    , RotateColumn 0 1
    , Rect 19 1
    , RotateColumn 34 2
    , RotateColumn 24 1
    , RotateColumn 23 1
    , RotateColumn 14 1
    , RotateColumn 9 2
    , RotateColumn 4 2
    , RotateRow 3 5
    , RotateRow 2 3
    , RotateRow 1 7
    , RotateRow 0 5
    , RotateColumn 0 2
    , Rect 3 2
    , RotateColumn 16 2
    , RotateRow 3 27
    , RotateRow 2 5
    , RotateRow 0 20
    , RotateColumn 8 2
    , RotateColumn 7 1
    , RotateColumn 5 1
    , RotateColumn 3 3
    , RotateColumn 2 1
    , RotateColumn 1 2
    , RotateColumn 0 1
    , Rect 9 1
    , RotateRow 4 42
    , RotateRow 3 40
    , RotateRow 1 30
    , RotateRow 0 40
    , RotateColumn 37 2
    , RotateColumn 36 3
    , RotateColumn 35 1
    , RotateColumn 33 1
    , RotateColumn 32 1
    , RotateColumn 31 3
    , RotateColumn 30 1
    , RotateColumn 28 1
    , RotateColumn 27 1
    , RotateColumn 25 1
    , RotateColumn 23 3
    , RotateColumn 22 1
    , RotateColumn 21 1
    , RotateColumn 20 1
    , RotateColumn 18 1
    , RotateColumn 17 1
    , RotateColumn 16 3
    , RotateColumn 15 1
    , RotateColumn 13 1
    , RotateColumn 12 1
    , RotateColumn 11 2
    , RotateColumn 10 1
    , RotateColumn 8 1
    , RotateColumn 7 2
    , RotateColumn 5 1
    , RotateColumn 3 3
    , RotateColumn 2 1
    , RotateColumn 1 1
    , RotateColumn 0 1
    , Rect 39 1
    , RotateColumn 44 2
    , RotateColumn 42 2
    , RotateColumn 35 5
    , RotateColumn 34 2
    , RotateColumn 32 2
    , RotateColumn 29 2
    , RotateColumn 25 5
    , RotateColumn 24 2
    , RotateColumn 19 2
    , RotateColumn 15 4
    , RotateColumn 14 2
    , RotateColumn 12 3
    , RotateColumn 9 2
    , RotateColumn 5 5
    , RotateColumn 4 2
    , RotateRow 5 5
    , RotateRow 4 38
    , RotateRow 3 10
    , RotateRow 2 46
    , RotateRow 1 10
    , RotateColumn 48 4
    , RotateColumn 47 3
    , RotateColumn 46 3
    , RotateColumn 45 1
    , RotateColumn 43 1
    , RotateColumn 37 5
    , RotateColumn 36 5
    , RotateColumn 35 4
    , RotateColumn 33 1
    , RotateColumn 32 5
    , RotateColumn 31 5
    , RotateColumn 28 5
    , RotateColumn 27 5
    , RotateColumn 26 3
    , RotateColumn 25 4
    , RotateColumn 23 1
    , RotateColumn 17 5
    , RotateColumn 16 5
    , RotateColumn 13 1
    , RotateColumn 12 5
    , RotateColumn 11 5
    , RotateColumn 3 1
    , RotateColumn 0 1
    ]
