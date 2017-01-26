module Main exposing (main)

import Html exposing (Html, ul, li, text, strong, button, div, beginnerProgram)
import Html.Events exposing (onClick)
import Model exposing (..)
import DayOne
import DayTwo
import DayThree
import DayFour
import DayFive
import DaySix
import DaySeven
import DayEight
import DayNine
import DayTen
import DayEleven
import DayTwelve
import DayThirteen
import DayFourteen
import DayFifteen
import DaySixteen
import DaySeventeen
import DayEighteen


type Model
    = Model (List QandA)


type Msg
    = Calculate Int


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }


view : Model -> Html Msg
view (Model answers) =
    div []
        [ text "N.B. For answers that take a long time to calculate, check the console for progress"
        , ul [] (List.indexedMap toLi answers)
        ]


update : Msg -> Model -> Model
update (Calculate i) (Model answers) =
    let
        before =
            List.take i answers

        after =
            List.drop (i + 1) answers

        toCalculate =
            List.drop i answers |> List.head
    in
        case toCalculate of
            Nothing ->
                Model answers

            Just (QandA q a) ->
                case a of
                    Calculated _ ->
                        Model answers

                    Uncalculated fn ->
                        Model (before ++ [ QandA q (Calculated (fn ())) ] ++ after)


toLi : Int -> QandA -> Html Msg
toLi i (QandA (Question question) answer) =
    let
        answerHtml =
            case answer of
                Uncalculated uncalc ->
                    button [ onClick (Calculate i) ] [ text "Calculate" ]

                Calculated calc ->
                    text calc
    in
        li [] [ strong [] [ text question ], text ": ", answerHtml ]


init : Model
init =
    Model
        ([]
            ++ DayOne.answers
            ++ DayTwo.answers
            ++ DayThree.answers
            ++ DayFour.answers
            ++ DayFive.answers
            ++ DaySix.answers
            ++ DaySeven.answers
            ++ DayEight.answers
            ++ DayNine.answers
            ++ DayTen.answers
            ++ DayEleven.answers
            ++ DayTwelve.answers
            ++ DayThirteen.answers
            ++ DayFourteen.answers
            ++ DayFifteen.answers
            ++ DaySixteen.answers
            ++ DaySeventeen.answers
            ++ DayEighteen.answers
        )
