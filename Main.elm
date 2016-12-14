module Main exposing (..)

import Html exposing (Html, ul, li, text, strong)
import DayOne
import DayTwo
import DayThree


main : Html msg
main =
    answers
        |> List.map toLi
        |> ul []


toLi : ( String, String ) -> Html msg
toLi ( name, answer ) =
    li []
        [ strong [] [ (text name) ]
        , text (": " ++ answer)
        ]


answers : List ( String, String )
answers =
    DayOne.answers
        ++ DayTwo.answers
        ++ DayThree.answers
