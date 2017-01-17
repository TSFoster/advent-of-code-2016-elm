module DayFourteen exposing (answers)

import Model exposing (..)
import MD5
import Dict exposing (Dict)


answers : List QandA
answers =
    [ QandA (Question "Day 14 part 1") part1
    , QandA (Question "Day 14 part 2 (WARNING: takes a long time)") part2
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            get64th MD5.hex
                |> toString
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            get64th stretchedHex
                |> toString
        )


type alias Memo =
    Dict Int Hash


type alias Hash =
    String


stretchedHex : String -> Hash
stretchedHex =
    let
        stretch : Int -> String -> Hash
        stretch n str =
            if n <= 0 then
                str
            else
                stretch (n - 1) (MD5.hex str)
    in
        stretch 2017 << Debug.log "Hashing"


get64th : (String -> Hash) -> Int
get64th hashFn =
    getNth hashFn Dict.empty 64 -1


getNth : (String -> Hash) -> Memo -> Int -> Int -> Int
getNth hashFn memo n i =
    if n <= 0 then
        i
    else
        let
            ( newMemo, newI ) =
                nextValid hashFn memo i
        in
            getNth hashFn newMemo (n - 1) newI


nextValid : (String -> Hash) -> Memo -> Int -> ( Memo, Int )
nextValid hashFn memo i =
    let
        j =
            i + 1

        ( newMemo, nextIsValid ) =
            isValid hashFn memo j
    in
        if nextIsValid then
            ( newMemo, j )
        else
            nextValid hashFn newMemo j


isValid : (String -> Hash) -> Memo -> Int -> ( Memo, Bool )
isValid hashFn memo i =
    let
        ( newMemo, key ) =
            hash hashFn memo i

        maybeChar =
            sameElemThreeTimes (String.toList key)
    in
        maybeChar
            |> Maybe.map (List.repeat 5 >> String.fromList)
            |> Maybe.map (checkForStringInHashes hashFn newMemo (i + 1) (i + 1000))
            |> Maybe.withDefault ( newMemo, False )


checkForStringInHashes : (String -> Hash) -> Memo -> Int -> Int -> String -> ( Memo, Bool )
checkForStringInHashes hashFn memo from to str =
    if from > to then
        ( memo, False )
    else
        let
            ( newMemo, key ) =
                hash hashFn memo from
        in
            if String.contains str key then
                ( newMemo, True )
            else
                checkForStringInHashes hashFn newMemo (from + 1) to str


sameElemThreeTimes : List a -> Maybe a
sameElemThreeTimes list =
    case list of
        a :: b :: c :: rest ->
            if a == b && b == c then
                Just a
            else
                sameElemThreeTimes (b :: c :: rest)

        _ ->
            Nothing


hash : (String -> Hash) -> Memo -> Int -> ( Memo, Hash )
hash hashFn memo i =
    let
        maybeVal =
            Dict.get i memo
    in
        case maybeVal of
            Nothing ->
                let
                    val =
                        hashFn (salt ++ toString i)
                in
                    ( Dict.insert i val memo, val )

            Just val ->
                ( memo, val )


salt : String
salt =
    "qzyelonm"
