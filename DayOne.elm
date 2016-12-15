module DayOne exposing (answers)

import Model exposing (..)


answers : List QandA
answers =
    [ QandA (Question "Day 1 part 1") part1
    , QandA (Question "Day 1 part 2") part2
    ]


answer : Position -> String
answer position =
    let
        ( x, y ) =
            position
    in
        abs x
            + abs y
            |> toString


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            answer (finalPosition instructions).position
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            (finalPosition instructions).firstDuplicate
                |> Maybe.map answer
                |> Maybe.withDefault "No such place"
        )


finalPosition : List Instruction -> State
finalPosition instructions =
    List.foldl update init instructions


type Instruction
    = L Int
    | R Int
    | S Int


type Facing
    = North
    | East
    | South
    | West


type alias Position =
    ( Int, Int )


type alias State =
    { position : Position
    , facing : Facing
    , visited : List Position
    , firstDuplicate : Maybe Position
    }


init : State
init =
    { position = ( 0, 0 )
    , facing = North
    , visited = []
    , firstDuplicate = Nothing
    }


update : Instruction -> State -> State
update instruction { position, facing, visited, firstDuplicate } =
    let
        newFacing =
            turn instruction facing

        distance =
            blocks instruction

        newPosition =
            move (S 1) newFacing position

        newFirstDuplicate =
            case firstDuplicate of
                Nothing ->
                    if List.member newPosition visited then
                        Just newPosition
                    else
                        Nothing

                Just duplicate ->
                    Just duplicate
    in
        if distance <= 1 then
            { position = newPosition
            , facing = newFacing
            , visited = visited
            , firstDuplicate = newFirstDuplicate
            }
        else
            update (S (distance - 1))
                { position = newPosition
                , facing = newFacing
                , visited = newPosition :: visited
                , firstDuplicate = newFirstDuplicate
                }


blocks : Instruction -> Int
blocks instruction =
    case instruction of
        L x ->
            x

        R x ->
            x

        S x ->
            x


move : Instruction -> Facing -> Position -> Position
move instruction facing ( x, y ) =
    case facing of
        North ->
            ( x, y + blocks instruction )

        East ->
            ( x + blocks instruction, y )

        South ->
            ( x, y - blocks instruction )

        West ->
            ( x - blocks instruction, y )


turn : Instruction -> Facing -> Facing
turn instruction facing =
    case instruction of
        L _ ->
            case facing of
                North ->
                    West

                East ->
                    North

                South ->
                    East

                West ->
                    South

        R _ ->
            case facing of
                North ->
                    East

                East ->
                    South

                South ->
                    West

                West ->
                    North

        S _ ->
            facing


instructions : List Instruction
instructions =
    [ L 5
    , R 1
    , R 4
    , L 5
    , L 4
    , R 3
    , R 1
    , L 1
    , R 4
    , R 5
    , L 1
    , L 3
    , R 4
    , L 2
    , L 4
    , R 2
    , L 4
    , L 1
    , R 3
    , R 1
    , R 1
    , L 1
    , R 1
    , L 5
    , R 5
    , R 2
    , L 5
    , R 2
    , R 1
    , L 2
    , L 4
    , L 4
    , R 191
    , R 2
    , R 5
    , R 1
    , L 1
    , L 2
    , R 5
    , L 2
    , L 3
    , R 4
    , L 1
    , L 1
    , R 1
    , R 50
    , L 1
    , R 1
    , R 76
    , R 5
    , R 4
    , R 2
    , L 5
    , L 3
    , L 5
    , R 2
    , R 1
    , L 1
    , R 2
    , L 3
    , R 4
    , R 2
    , L 1
    , L 1
    , R 4
    , L 1
    , L 1
    , R 185
    , R 1
    , L 5
    , L 4
    , L 5
    , L 3
    , R 2
    , R 3
    , R 1
    , L 5
    , R 1
    , L 3
    , L 2
    , L 2
    , R 5
    , L 1
    , L 1
    , L 3
    , R 1
    , R 4
    , L 2
    , L 1
    , L 1
    , L 3
    , L 4
    , R 5
    , L 2
    , R 3
    , R 5
    , R 1
    , L 4
    , R 5
    , L 3
    , R 3
    , R 3
    , R 1
    , R 1
    , R 5
    , R 2
    , L 2
    , R 5
    , L 5
    , L 4
    , R 4
    , R 3
    , R 5
    , R 1
    , L 3
    , R 1
    , L 2
    , L 2
    , R 3
    , R 4
    , L 1
    , R 4
    , L 1
    , R 4
    , R 3
    , L 1
    , L 4
    , L 1
    , L 5
    , L 2
    , R 2
    , L 1
    , R 1
    , L 5
    , L 3
    , R 4
    , L 1
    , R 5
    , L 5
    , L 5
    , L 1
    , L 3
    , R 1
    , R 5
    , L 2
    , L 4
    , L 5
    , L 1
    , L 1
    , L 2
    , R 5
    , R 5
    , L 4
    , R 3
    , L 2
    , L 1
    , L 3
    , L 4
    , L 5
    , L 5
    , L 2
    , R 4
    , R 3
    , L 5
    , R 4
    , R 2
    , R 1
    , L 5
    ]
