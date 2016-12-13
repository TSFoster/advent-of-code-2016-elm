module DayOne exposing (answers)


answers : List ( String, String )
answers =
    [ ( "Day 1 part 1", part1 )
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


part1 : String
part1 =
    answer finalPosition.position


finalPosition : State
finalPosition =
    List.foldl update init instructions


type Instruction
    = L Int
    | R Int


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
    }


init : State
init =
    { position = ( 0, 0 )
    , facing = North
    }


update : Instruction -> State -> State
update instruction { position, facing } =
    let
        newFacing =
            turn instruction facing
    in
        { position = move instruction newFacing position
        , facing = newFacing
        }


blocks : Instruction -> Int
blocks instruction =
    case instruction of
        L x ->
            x

        R x ->
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
