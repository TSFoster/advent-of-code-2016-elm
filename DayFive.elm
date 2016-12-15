module DayFive exposing (answers)

import MD5 exposing (hex)
import Model exposing (..)
import Debug exposing (log)


answers : List QandA
answers =
    [ QandA (Question "Day 5 part 1") part1
    , QandA (Question "Day 5 part 2") part2
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            getPassword "uqwqemis"
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            getSecondPassword "uqwqemis"
        )


getPassword : String -> String
getPassword doorId =
    findNextChars 8 doorId 0 |> String.fromList


getSecondPassword : String -> String
getSecondPassword doorId =
    findCharsForIndexes (List.repeat 8 ' ') doorId 4515059 |> String.fromList


findNextChars : Int -> String -> Int -> List Char
findNextChars count prefix i =
    if count <= 0 then
        []
    else
        let
            ( next, _, j ) =
                findNextChar prefix i
        in
            (log ("Found character with index " ++ toString j ++ "! " ++ toString (count - 1) ++ " more to go") next)
                :: findNextChars (count - 1) prefix (j + 1)


findCharsForIndexes : List Char -> String -> Int -> List Char
findCharsForIndexes list prefix i =
    if not (List.member ' ' list) then
        list
    else
        let
            ( index, next, j ) =
                findNextChar prefix i
        in
            case String.fromChar index |> String.toInt |> Result.toMaybe of
                Nothing ->
                    findCharsForIndexes list prefix (j + 1)

                Just pos ->
                    let
                        newList =
                            log ("Status as of " ++ toString j) (setCharIfEmpty pos next list)
                    in
                        findCharsForIndexes newList prefix (j + 1)


findNextChar : String -> Int -> ( Char, Char, Int )
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
            ( getPos 6 hash, getPos 7 hash, i )
        else
            findNextChar prefix (i + 1)


getPos : Int -> String -> Char
getPos i =
    String.toList
        >> List.drop (i - 1)
        >> List.head
        >> Maybe.withDefault ' '


setCharIfEmpty : Int -> Char -> List Char -> List Char
setCharIfEmpty pos char list =
    let
        before =
            List.take pos list

        atPos =
            List.drop pos list |> List.head

        after =
            List.drop (pos + 1) list
    in
        case atPos of
            Nothing ->
                list

            Just c ->
                case c of
                    ' ' ->
                        before ++ [ char ] ++ after

                    _ ->
                        list
