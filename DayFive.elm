module DayFive exposing (answers)

import MD5 exposing (hex)
import Model exposing (..)
import Debug exposing (log)


answers : List QandA
answers =
    [ QandA (Question "Day 5 part 1") part1
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            getPassword "uqwqemis"
        )


getPassword : String -> String
getPassword doorId =
    findNextChars 8 doorId 0 |> String.fromList


findNextChars : Int -> String -> Int -> List Char
findNextChars count prefix i =
    if count <= 0 then
        []
    else
        let
            ( next, j ) =
                findNextChar prefix i
        in
            (log ("Found character with index " ++ toString j ++ "! " ++ toString (count - 1) ++ " more to go") next)
                :: findNextChars (count - 1) prefix (j + 1)


findNextChar : String -> Int -> ( Char, Int )
findNextChar prefix i =
    let
        logProgress =
            if i % 10000 == 0 then
                log "Checking indexes" (toString i ++ " through " ++ toString (i + 9999))
            else
                ""

        hash =
            hex (prefix ++ toString i)
    in
        if hash |> String.startsWith "00000" then
            ( sixthPos hash, i )
        else
            findNextChar prefix (i + 1)


sixthPos : String -> Char
sixthPos =
    String.toList
        >> List.drop 5
        >> List.head
        >> Maybe.withDefault ' '
