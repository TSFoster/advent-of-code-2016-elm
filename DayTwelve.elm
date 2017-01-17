module DayTwelve exposing (answers)

import Model exposing (..)
import Dict exposing (Dict)


answers : List QandA
answers =
    [ QandA (Question "Day 12 part 1") part1
    , QandA (Question "Day 12 part 2") part2
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            toString <| registerAAfterExecution init
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            let
                newInit =
                    { init | registers = set "c" 1 init.registers }
            in
                toString <| registerAAfterExecution newInit
        )


type alias Model =
    { previous : List Instruction
    , next : List Instruction
    , registers : Registers
    }


type alias Registers =
    Dict RegisterName Int


type alias RegisterName =
    String


type Val
    = Value Int
    | Register RegisterName


type Instruction
    = Cpy Val RegisterName
    | Inc RegisterName
    | Dec RegisterName
    | Jnz Val Int


registerAAfterExecution : Model -> Int
registerAAfterExecution =
    finalState
        >> .registers
        >> get (Register "a")


finalState : Model -> Model
finalState model =
    case step model of
        Nothing ->
            model

        Just newModel ->
            finalState newModel


step : Model -> Maybe Model
step ({ next, registers } as model) =
    case List.head next of
        Nothing ->
            Nothing

        Just instruction ->
            let
                newModel =
                    shiftInstructions 1 model
            in
                case instruction of
                    Cpy val reg ->
                        let
                            newRegisters =
                                case val of
                                    Value i ->
                                        set reg i registers

                                    Register r ->
                                        copy r reg registers
                        in
                            Just { newModel | registers = newRegisters }

                    Inc reg ->
                        Just
                            { newModel
                                | registers = update reg ((+) 1) registers
                            }

                    Dec reg ->
                        Just
                            { newModel
                                | registers = update reg ((+) -1) registers
                            }

                    Jnz val amount ->
                        if get val registers == 0 then
                            Just newModel
                        else
                            Just <| shiftInstructions amount model


shiftInstructions : Int -> Model -> Model
shiftInstructions amount ({ previous, next } as model) =
    case compare amount 0 of
        EQ ->
            model

        LT ->
            case previous of
                [] ->
                    model

                prev :: rest ->
                    shiftInstructions (amount + 1)
                        { model
                            | previous = rest
                            , next = prev :: next
                        }

        GT ->
            case next of
                [] ->
                    model

                nxt :: rest ->
                    shiftInstructions (amount - 1)
                        { model
                            | previous = nxt :: previous
                            , next = rest
                        }


get : Val -> Registers -> Int
get val registers =
    case val of
        Value i ->
            i

        Register r ->
            Dict.get r registers
                |> Maybe.withDefault 0


set : RegisterName -> Int -> Registers -> Registers
set reg val =
    update reg (always val)


update : RegisterName -> (Int -> Int) -> Registers -> Registers
update key fn =
    Dict.update key (Just << fn << (Maybe.withDefault 0))


copy : RegisterName -> RegisterName -> Registers -> Registers
copy from to dict =
    set to (get (Register from) dict) dict


init : Model
init =
    { previous = []
    , next =
        [ Cpy (Value 1) "a"
        , Cpy (Value 1) "a"
        , Cpy (Value 1) "b"
        , Cpy (Value 26) "d"
        , Jnz (Register "c") 2
        , Jnz (Value 1) 5
        , Cpy (Value 7) "c"
        , Inc "d"
        , Dec "c"
        , Jnz (Register "c") -2
        , Cpy (Register "a") "c"
        , Inc "a"
        , Dec "b"
        , Jnz (Register "b") -2
        , Cpy (Register "c") "b"
        , Dec "d"
        , Jnz (Register "d") -6
        , Cpy (Value 13) "c"
        , Cpy (Value 14) "d"
        , Inc "a"
        , Dec "d"
        , Jnz (Register "d") -2
        , Dec "c"
        , Jnz (Register "c") -5
        ]
    , registers = Dict.empty
    }
