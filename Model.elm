module Model exposing (..)


type QandA
    = QandA Question Answer


type Question
    = Question String


type Answer
    = Calculated String
    | Uncalculated (() -> String)
