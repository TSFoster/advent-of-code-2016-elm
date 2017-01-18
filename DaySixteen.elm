module DaySixteen exposing (answers)

import Model exposing (..)


answers : List QandA
answers =
    [ QandA (Question "Day 16 part 1") part1
    , QandA (Question "Day 16 part 2 (WARNING: takes a long time)") part2
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            input
                |> toBinary
                |> checksumForExpanded 272
                |> binaryToString
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            input
                |> toBinary
                |> checksumForExpanded 35651584
                |> binaryToString
        )


type Binary
    = Zero
    | One


checksumForExpanded : Int -> List Binary -> List Binary
checksumForExpanded i =
    expandDataToFit i >> checksum


checksum : List Binary -> List Binary
checksum data =
    let
        step =
            checksumStep data
    in
        if List.length step % 2 == 0 then
            checksum step
        else
            step


checksumStep : List Binary -> List Binary
checksumStep =
    pairs
        >> List.map (uncurry (==))
        >> List.map boolToBinary


pairs : List Binary -> List ( Binary, Binary )
pairs data =
    let
        pairsMemo : List ( Binary, Binary ) -> List Binary -> List ( Binary, Binary )
        pairsMemo memo data =
            case data of
                x :: y :: rest ->
                    pairsMemo (( x, y ) :: memo) rest

                _ ->
                    List.reverse memo
    in
        pairsMemo [] data


expandDataToFit : Int -> List Binary -> List Binary
expandDataToFit size data =
    if List.length data >= size then
        List.take size data
    else
        expandDataToFit size (runExpansion data)


runExpansion : List Binary -> List Binary
runExpansion data =
    data ++ Zero :: List.reverse (List.map flip data)


flip : Binary -> Binary
flip b =
    case b of
        Zero ->
            One

        One ->
            Zero


binaryToString : List Binary -> String
binaryToString =
    List.map toChar >> String.fromList


toChar : Binary -> Char
toChar b =
    case b of
        Zero ->
            '0'

        One ->
            '1'


toBinary : String -> List Binary
toBinary =
    String.toList >> List.filterMap charToBinary


charToBinary : Char -> Maybe Binary
charToBinary char =
    case char of
        '1' ->
            Just One

        '0' ->
            Just Zero

        _ ->
            Nothing


boolToBinary : Bool -> Binary
boolToBinary b =
    case b of
        True ->
            One

        False ->
            Zero


input : String
input =
    "11011110011011101"
