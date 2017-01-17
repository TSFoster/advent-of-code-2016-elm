module DayFourteen exposing (answers)

import Model exposing (..)
import MD5
import Dict exposing (Dict)


answers : List QandA
answers =
    [ QandA (Question "Day 14 part 1") part1
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            getNth Dict.empty 64 -1
                |> toString
        )


type alias Memo =
    Dict Int Hash


type alias Hash =
    String


getNth : Memo -> Int -> Int -> Int
getNth memo n i =
    if n <= 0 then
        i
    else
        let
            ( newMemo, newI ) =
                nextValid memo i
        in
            getNth newMemo (n - 1) newI


nextValid : Memo -> Int -> ( Memo, Int )
nextValid memo i =
    let
        j =
            i + 1

        ( newMemo, nextIsValid ) =
            isValid memo j
    in
        if nextIsValid then
            ( newMemo, j )
        else
            nextValid newMemo j


isValid : Memo -> Int -> ( Memo, Bool )
isValid memo i =
    let
        ( newMemo, key ) =
            hash memo i

        maybeChar =
            sameElemThreeTimes (String.toList key)
    in
        maybeChar
            |> Maybe.map (List.repeat 5 >> String.fromList)
            |> Maybe.map (checkForStringInHashes newMemo (i + 1) (i + 1000))
            |> Maybe.withDefault ( newMemo, False )


checkForStringInHashes : Memo -> Int -> Int -> String -> ( Memo, Bool )
checkForStringInHashes memo from to str =
    if from > to then
        ( memo, False )
    else
        let
            ( newMemo, key ) =
                hash memo from
        in
            if String.contains str key then
                ( newMemo, True )
            else
                checkForStringInHashes newMemo (from + 1) to str


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


hash : Memo -> Int -> ( Memo, Hash )
hash memo i =
    let
        maybeVal =
            Dict.get i memo
    in
        case maybeVal of
            Nothing ->
                let
                    val =
                        MD5.hex (salt ++ toString i)
                in
                    ( Dict.insert i val memo, val )

            Just val ->
                ( memo, val )


salt : String
salt =
    "qzyelonm"
