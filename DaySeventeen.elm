module DaySeventeen exposing (answers)

import Model exposing (..)
import Set exposing (Set)
import Char
import MD5


answers : List QandA
answers =
    [ QandA (Question "Day 17 part 1") part1
    , QandA (Question "Day 17 part 2 (WARNING: takes a long time)") part2
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            String.fromList <| shortestPath [ init ]
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            toString <| longestPath 0 [ init ]
        )


type alias Path =
    List Direction


type alias Doors =
    List Direction


type alias Direction =
    Char


type alias Position =
    ( Int, Int )


type alias State =
    ( Position, Path )


longestPath : Int -> List State -> Int
longestPath soFar states =
    case states of
        [] ->
            soFar

        state :: rest ->
            if reachedEnd state then
                let
                    newPathLength =
                        pathLength state

                    ( newSoFar, _ ) =
                        ( max soFar newPathLength, newPathLength )
                            |> Debug.log "(Best path, new path found)"
                in
                    longestPath newSoFar rest
            else
                longestPath soFar (step state ++ rest)


pathLength : State -> Int
pathLength =
    List.length << Tuple.second


shortestPath : List State -> Path
shortestPath states =
    case List.filter reachedEnd states of
        ( _, path ) :: _ ->
            List.reverse path

        [] ->
            shortestPath (List.concatMap step states)


reachedEnd : State -> Bool
reachedEnd ( pos, _ ) =
    pos == ( 3, 3 )


step : State -> List State
step (( _, path ) as state) =
    List.filterMap (move state) (doorsOpen path)


move : State -> Direction -> Maybe State
move ( ( x, y ), path ) dir =
    case dir of
        'U' ->
            if y <= 0 then
                Nothing
            else
                Just ( ( x, y - 1 ), dir :: path )

        'D' ->
            if y >= 3 then
                Nothing
            else
                Just ( ( x, y + 1 ), dir :: path )

        'L' ->
            if x <= 0 then
                Nothing
            else
                Just ( ( x - 1, y ), dir :: path )

        'R' ->
            if x >= 3 then
                Nothing
            else
                Just ( ( x + 1, y ), dir :: path )

        _ ->
            Nothing


doorsOpen : Path -> Doors
doorsOpen path =
    List.reverse path
        |> String.fromList
        |> (++) passcode
        |> MD5.hex
        |> String.toList
        |> List.take 4
        |> List.map (\c -> not <| c == 'a' || Char.isDigit c)
        |> List.map2 (,) [ 'U', 'D', 'L', 'R' ]
        |> List.filter Tuple.second
        |> List.map Tuple.first


removeIf : Bool -> comparable -> Set comparable -> Set comparable
removeIf b a =
    if b then
        Set.remove a
    else
        identity


init : State
init =
    ( ( 0, 0 ), [] )


passcode : String
passcode =
    "pgflpeqp"
