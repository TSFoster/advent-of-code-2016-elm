module DayTen exposing (answers)

import Model exposing (..)
import Dict exposing (Dict)
import Debug


answers : List QandA
answers =
    [ QandA (Question "Day 10 part 1") part1
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            initFromRules input
                |> whichBotCompares61and17
                |> Maybe.map toString
                |> Maybe.withDefault "61 and 17 were not compared"
        )


type alias Bot =
    { chips : HeldChips
    , giveLowerTo : Actor
    , giveHigherTo : Actor
    }


type alias Output =
    List Int


type alias State =
    { bots : Dict Int Bot
    , outputs : Dict Int Output
    }


type HeldChips
    = None
    | One Int
    | Two Int Int


type NewState
    = New State
    | Unchanged


type Actor
    = Robot Int
    | Output Int


whichBotCompares61and17 : State -> Maybe Int
whichBotCompares61and17 state =
    let
        botHas61and17 { chips } =
            case chips of
                Two 17 61 ->
                    True

                _ ->
                    False

        botWith61and17 =
            state.bots
                |> Dict.filter (always botHas61and17)
                |> Dict.keys
                |> List.head
    in
        case botWith61and17 of
            Nothing ->
                case step state of
                    New newState ->
                        whichBotCompares61and17 newState

                    Unchanged ->
                        Nothing

            Just id ->
                Just id


step : State -> NewState
step ({ bots, outputs } as state) =
    let
        hasTwoChips { chips } =
            case chips of
                Two _ _ ->
                    True

                _ ->
                    False

        botWithTwoChips =
            bots
                |> Dict.filter (always hasTwoChips)
                |> Dict.toList
                |> List.head
    in
        case botWithTwoChips of
            Nothing ->
                Unchanged

            Just ( id, { chips, giveLowerTo, giveHigherTo } ) ->
                let
                    ( lower, higher ) =
                        case chips of
                            Two a b ->
                                ( a, b )

                            _ ->
                                Debug.crash "Shouldn't ever happen!" ( 0, 0 )
                in
                    state
                        |> removeChips id
                        |> give giveLowerTo lower
                        |> give giveHigherTo higher
                        |> New


give : Actor -> Int -> State -> State
give actor chip state =
    case actor of
        Robot id ->
            { state | bots = Dict.update id (Maybe.map (giveBotChip chip)) state.bots }

        Output id ->
            { state | outputs = Dict.update id (sendToOutput chip) state.outputs }


sendToOutput : Int -> Maybe Output -> Maybe Output
sendToOutput chip current =
    let
        output =
            current |> Maybe.withDefault []
    in
        Just <| chip :: output


giveBotChip : Int -> Bot -> Bot
giveBotChip chip bot =
    case bot.chips of
        None ->
            { bot | chips = One chip }

        One chip2 ->
            { bot | chips = Two (min chip chip2) (max chip chip2) }

        Two _ _ ->
            Debug.crash "LAME" bot


removeChips : Int -> State -> State
removeChips id state =
    let
        newBots =
            state.bots
                |> Dict.update id (Maybe.map (\bot -> { bot | chips = None }))
    in
        { state | bots = newBots }


initFromRules : List Rule -> State
initFromRules rules =
    let
        addBots rule ({ bots } as state) =
            case rule of
                InitialChip _ _ ->
                    state

                WithTwoChips id lower upper ->
                    { state | bots = Dict.insert id (Bot None lower upper) bots }

        addChips rule ({ bots } as state) =
            case rule of
                WithTwoChips _ _ _ ->
                    state

                InitialChip chip id ->
                    give (Robot id) chip state

        state =
            List.foldl addBots { bots = Dict.empty, outputs = Dict.empty } rules

        stateWithChips =
            List.foldl addChips state rules
    in
        stateWithChips


type Rule
    = WithTwoChips Int Actor Actor
    | InitialChip Int Int


input : List Rule
input =
    [ WithTwoChips 49 (Robot 118) (Robot 182)
    , WithTwoChips 192 (Robot 40) (Robot 177)
    , WithTwoChips 195 (Output 4) (Robot 130)
    , WithTwoChips 176 (Robot 32) (Robot 168)
    , WithTwoChips 152 (Robot 98) (Robot 36)
    , WithTwoChips 99 (Robot 194) (Robot 171)
    , InitialChip 61 49
    , WithTwoChips 109 (Robot 137) (Robot 82)
    , WithTwoChips 57 (Robot 150) (Robot 185)
    , WithTwoChips 117 (Robot 70) (Robot 189)
    , WithTwoChips 87 (Robot 145) (Robot 26)
    , WithTwoChips 104 (Robot 20) (Robot 153)
    , WithTwoChips 14 (Robot 203) (Robot 162)
    , WithTwoChips 76 (Robot 80) (Robot 67)
    , WithTwoChips 80 (Robot 159) (Robot 183)
    , WithTwoChips 0 (Robot 99) (Robot 167)
    , WithTwoChips 13 (Robot 35) (Robot 129)
    , WithTwoChips 74 (Robot 177) (Robot 108)
    , WithTwoChips 5 (Robot 189) (Robot 27)
    , WithTwoChips 28 (Output 0) (Robot 61)
    , WithTwoChips 19 (Robot 77) (Robot 181)
    , WithTwoChips 197 (Robot 26) (Robot 207)
    , WithTwoChips 128 (Robot 65) (Robot 90)
    , WithTwoChips 93 (Robot 117) (Robot 5)
    , WithTwoChips 139 (Robot 74) (Robot 170)
    , WithTwoChips 132 (Robot 199) (Robot 23)
    , WithTwoChips 3 (Robot 103) (Robot 95)
    , WithTwoChips 162 (Robot 45) (Robot 188)
    , WithTwoChips 163 (Robot 132) (Robot 23)
    , InitialChip 19 20
    , WithTwoChips 164 (Robot 95) (Robot 14)
    , WithTwoChips 204 (Robot 140) (Robot 42)
    , WithTwoChips 155 (Robot 37) (Robot 43)
    , WithTwoChips 32 (Robot 15) (Robot 0)
    , WithTwoChips 6 (Robot 2) (Robot 161)
    , WithTwoChips 112 (Robot 85) (Robot 12)
    , WithTwoChips 24 (Robot 100) (Robot 25)
    , WithTwoChips 62 (Robot 143) (Robot 39)
    , WithTwoChips 131 (Output 11) (Robot 159)
    , WithTwoChips 166 (Robot 81) (Robot 163)
    , WithTwoChips 209 (Output 5) (Robot 140)
    , WithTwoChips 108 (Robot 125) (Robot 136)
    , WithTwoChips 138 (Robot 13) (Robot 156)
    , WithTwoChips 144 (Robot 195) (Robot 130)
    , WithTwoChips 15 (Robot 31) (Robot 99)
    , WithTwoChips 135 (Robot 43) (Robot 148)
    , WithTwoChips 12 (Robot 30) (Robot 157)
    , WithTwoChips 85 (Robot 56) (Robot 30)
    , InitialChip 47 133
    , WithTwoChips 27 (Robot 17) (Robot 166)
    , InitialChip 5 32
    , WithTwoChips 149 (Robot 41) (Robot 88)
    , WithTwoChips 156 (Robot 129) (Robot 179)
    , InitialChip 73 146
    , WithTwoChips 64 (Robot 49) (Robot 151)
    , WithTwoChips 185 (Robot 191) (Robot 97)
    , WithTwoChips 173 (Robot 1) (Robot 66)
    , WithTwoChips 70 (Robot 133) (Robot 9)
    , WithTwoChips 123 (Robot 154) (Robot 53)
    , WithTwoChips 91 (Robot 113) (Robot 175)
    , WithTwoChips 154 (Robot 73) (Robot 173)
    , WithTwoChips 43 (Robot 172) (Robot 3)
    , WithTwoChips 113 (Robot 44) (Robot 11)
    , WithTwoChips 196 (Robot 52) (Robot 84)
    , InitialChip 37 118
    , WithTwoChips 83 (Output 9) (Robot 56)
    , WithTwoChips 150 (Robot 111) (Robot 191)
    , WithTwoChips 60 (Robot 19) (Robot 141)
    , InitialChip 23 143
    , WithTwoChips 198 (Robot 127) (Robot 87)
    , WithTwoChips 44 (Robot 193) (Robot 48)
    , WithTwoChips 7 (Robot 139) (Robot 178)
    , WithTwoChips 143 (Output 12) (Robot 120)
    , WithTwoChips 130 (Output 1) (Output 13)
    , WithTwoChips 56 (Output 10) (Robot 101)
    , WithTwoChips 30 (Robot 101) (Robot 190)
    , WithTwoChips 67 (Robot 183) (Robot 33)
    , WithTwoChips 92 (Robot 167) (Robot 113)
    , WithTwoChips 115 (Robot 197) (Robot 199)
    , WithTwoChips 69 (Robot 131) (Robot 80)
    , WithTwoChips 100 (Robot 136) (Robot 25)
    , WithTwoChips 11 (Robot 48) (Robot 201)
    , WithTwoChips 186 (Robot 6) (Robot 29)
    , WithTwoChips 17 (Robot 54) (Robot 81)
    , WithTwoChips 122 (Robot 64) (Robot 200)
    , InitialChip 71 176
    , WithTwoChips 103 (Robot 33) (Robot 57)
    , WithTwoChips 31 (Robot 122) (Robot 194)
    , WithTwoChips 29 (Robot 161) (Robot 79)
    , WithTwoChips 95 (Robot 57) (Robot 203)
    , WithTwoChips 171 (Robot 109) (Robot 193)
    , WithTwoChips 51 (Robot 106) (Robot 19)
    , WithTwoChips 48 (Robot 206) (Robot 192)
    , WithTwoChips 127 (Robot 168) (Robot 145)
    , WithTwoChips 36 (Robot 128) (Robot 90)
    , InitialChip 3 64
    , WithTwoChips 22 (Output 6) (Robot 131)
    , WithTwoChips 39 (Robot 120) (Robot 147)
    , WithTwoChips 61 (Output 17) (Robot 195)
    , WithTwoChips 194 (Robot 200) (Robot 109)
    , WithTwoChips 180 (Robot 5) (Robot 72)
    , WithTwoChips 86 (Robot 164) (Robot 119)
    , WithTwoChips 65 (Robot 28) (Robot 121)
    , WithTwoChips 68 (Robot 123) (Robot 184)
    , WithTwoChips 63 (Robot 58) (Robot 196)
    , WithTwoChips 98 (Robot 157) (Robot 36)
    , WithTwoChips 157 (Robot 190) (Robot 128)
    , WithTwoChips 18 (Robot 126) (Robot 7)
    , WithTwoChips 79 (Robot 124) (Robot 154)
    , WithTwoChips 40 (Robot 55) (Robot 186)
    , WithTwoChips 188 (Robot 96) (Robot 152)
    , WithTwoChips 165 (Robot 175) (Robot 18)
    , WithTwoChips 142 (Robot 79) (Robot 123)
    , WithTwoChips 81 (Robot 114) (Robot 163)
    , WithTwoChips 78 (Output 18) (Robot 202)
    , WithTwoChips 45 (Robot 97) (Robot 96)
    , WithTwoChips 23 (Robot 63) (Robot 196)
    , WithTwoChips 208 (Robot 176) (Robot 127)
    , WithTwoChips 206 (Robot 110) (Robot 40)
    , WithTwoChips 90 (Robot 121) (Robot 144)
    , WithTwoChips 46 (Output 16) (Robot 28)
    , WithTwoChips 167 (Robot 171) (Robot 44)
    , WithTwoChips 147 (Robot 78) (Robot 89)
    , WithTwoChips 116 (Robot 60) (Robot 2)
    , WithTwoChips 105 (Robot 87) (Robot 197)
    , WithTwoChips 133 (Robot 160) (Robot 149)
    , WithTwoChips 42 (Robot 22) (Robot 69)
    , WithTwoChips 137 (Robot 138) (Robot 134)
    , WithTwoChips 136 (Robot 142) (Robot 68)
    , WithTwoChips 75 (Output 20) (Robot 174)
    , WithTwoChips 178 (Robot 170) (Robot 24)
    , WithTwoChips 119 (Robot 14) (Robot 16)
    , WithTwoChips 158 (Robot 83) (Robot 85)
    , WithTwoChips 182 (Robot 187) (Robot 13)
    , WithTwoChips 110 (Robot 38) (Robot 55)
    , InitialChip 17 122
    , WithTwoChips 120 (Output 2) (Robot 78)
    , InitialChip 59 31
    , WithTwoChips 89 (Robot 202) (Robot 107)
    , WithTwoChips 184 (Robot 53) (Robot 71)
    , WithTwoChips 153 (Robot 180) (Robot 72)
    , WithTwoChips 106 (Robot 107) (Robot 77)
    , InitialChip 43 34
    , WithTwoChips 58 (Robot 18) (Robot 52)
    , WithTwoChips 111 (Robot 174) (Robot 158)
    , WithTwoChips 200 (Robot 151) (Robot 137)
    , WithTwoChips 10 (Robot 12) (Robot 98)
    , WithTwoChips 148 (Robot 3) (Robot 164)
    , WithTwoChips 47 (Robot 59) (Robot 155)
    , WithTwoChips 169 (Robot 69) (Robot 76)
    , WithTwoChips 38 (Robot 179) (Robot 116)
    , InitialChip 2 15
    , InitialChip 13 93
    , WithTwoChips 203 (Robot 185) (Robot 45)
    , WithTwoChips 72 (Robot 27) (Robot 166)
    , WithTwoChips 101 (Output 19) (Robot 46)
    , WithTwoChips 94 (Robot 119) (Robot 16)
    , WithTwoChips 35 (Robot 147) (Robot 21)
    , WithTwoChips 175 (Robot 11) (Robot 126)
    , WithTwoChips 54 (Robot 88) (Robot 114)
    , WithTwoChips 125 (Robot 29) (Robot 142)
    , WithTwoChips 201 (Robot 192) (Robot 74)
    , WithTwoChips 26 (Robot 91) (Robot 165)
    , InitialChip 53 62
    , WithTwoChips 16 (Robot 162) (Robot 188)
    , WithTwoChips 141 (Robot 181) (Robot 47)
    , WithTwoChips 41 (Robot 198) (Robot 105)
    , WithTwoChips 21 (Robot 89) (Robot 106)
    , InitialChip 31 208
    , WithTwoChips 187 (Robot 39) (Robot 35)
    , WithTwoChips 145 (Robot 92) (Robot 91)
    , WithTwoChips 55 (Robot 116) (Robot 6)
    , WithTwoChips 159 (Output 8) (Robot 8)
    , InitialChip 11 70
    , WithTwoChips 50 (Robot 155) (Robot 135)
    , WithTwoChips 2 (Robot 141) (Robot 102)
    , WithTwoChips 168 (Robot 0) (Robot 92)
    , WithTwoChips 189 (Robot 9) (Robot 17)
    , WithTwoChips 53 (Robot 173) (Robot 71)
    , WithTwoChips 129 (Robot 21) (Robot 51)
    , WithTwoChips 52 (Robot 7) (Robot 84)
    , WithTwoChips 172 (Robot 67) (Robot 103)
    , WithTwoChips 33 (Robot 205) (Robot 150)
    , WithTwoChips 37 (Robot 76) (Robot 172)
    , WithTwoChips 190 (Robot 46) (Robot 65)
    , WithTwoChips 199 (Robot 207) (Robot 63)
    , WithTwoChips 179 (Robot 51) (Robot 60)
    , WithTwoChips 20 (Robot 93) (Robot 180)
    , WithTwoChips 34 (Robot 208) (Robot 198)
    , WithTwoChips 151 (Robot 182) (Robot 138)
    , WithTwoChips 71 (Robot 66) (Robot 94)
    , WithTwoChips 160 (Robot 34) (Robot 41)
    , WithTwoChips 170 (Robot 108) (Robot 100)
    , WithTwoChips 4 (Robot 42) (Robot 169)
    , WithTwoChips 77 (Robot 204) (Robot 4)
    , WithTwoChips 161 (Robot 102) (Robot 124)
    , WithTwoChips 177 (Robot 186) (Robot 125)
    , WithTwoChips 181 (Robot 4) (Robot 59)
    , WithTwoChips 97 (Robot 112) (Robot 10)
    , WithTwoChips 134 (Robot 156) (Robot 38)
    , WithTwoChips 205 (Robot 75) (Robot 111)
    , WithTwoChips 1 (Robot 148) (Robot 86)
    , WithTwoChips 146 (Robot 104) (Robot 153)
    , WithTwoChips 174 (Output 3) (Robot 83)
    , WithTwoChips 102 (Robot 47) (Robot 50)
    , WithTwoChips 124 (Robot 50) (Robot 73)
    , WithTwoChips 207 (Robot 165) (Robot 58)
    , WithTwoChips 118 (Robot 62) (Robot 187)
    , WithTwoChips 140 (Output 7) (Robot 22)
    , WithTwoChips 8 (Output 14) (Robot 75)
    , InitialChip 41 117
    , WithTwoChips 121 (Robot 61) (Robot 144)
    , WithTwoChips 88 (Robot 105) (Robot 115)
    , WithTwoChips 114 (Robot 115) (Robot 132)
    , WithTwoChips 66 (Robot 86) (Robot 94)
    , InitialChip 29 146
    , WithTwoChips 73 (Robot 135) (Robot 1)
    , WithTwoChips 84 (Robot 178) (Robot 24)
    , WithTwoChips 191 (Robot 158) (Robot 112)
    , WithTwoChips 202 (Output 15) (Robot 209)
    , WithTwoChips 183 (Robot 8) (Robot 205)
    , InitialChip 7 160
    , WithTwoChips 126 (Robot 201) (Robot 139)
    , WithTwoChips 107 (Robot 209) (Robot 204)
    , WithTwoChips 59 (Robot 169) (Robot 37)
    , InitialChip 67 104
    , WithTwoChips 9 (Robot 149) (Robot 54)
    , WithTwoChips 82 (Robot 134) (Robot 110)
    , WithTwoChips 25 (Robot 68) (Robot 184)
    , WithTwoChips 193 (Robot 82) (Robot 206)
    , WithTwoChips 96 (Robot 10) (Robot 152)
    ]
