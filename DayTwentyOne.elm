module DayTwentyOne exposing (answers)

import Model exposing (..)


answers : List QandA
answers =
    [ QandA (Question "Day 21 part 1") part1
    , QandA (Question "Day 21 part 2") part2
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            String.fromList <| scramble password input
        )


part2 : Answer
part2 =
    Uncalculated
        (\() ->
            String.fromList <| unscramble (String.toList "fbgdceah") input
        )


type Instruction
    = MovePosition Int Int
    | ReversePositions Int Int
    | SwapPosition Int Int
    | SwapLetter Char Char
    | RotateBasedOn Char
    | RotateRight Int
    | RotateLeft Int
    | UnrotateBasedOn Char


unscramble : List Char -> List Instruction -> List Char
unscramble password instructions =
    List.foldr process password (List.map inverse instructions)


scramble : List Char -> List Instruction -> List Char
scramble =
    List.foldl process


process : Instruction -> List Char -> List Char
process instruction chars =
    case instruction of
        MovePosition a b ->
            let
                without =
                    List.take a chars ++ List.drop (a + 1) chars

                atPos =
                    List.drop a chars |> List.take 1

                before =
                    List.take b without

                after =
                    List.drop b without
            in
                before ++ atPos ++ after

        ReversePositions a b ->
            let
                before =
                    List.take a chars

                after =
                    List.drop (b + 1) chars

                toReverse =
                    List.drop a chars |> List.take (b - a + 1)
            in
                before ++ List.reverse toReverse ++ after

        SwapPosition c d ->
            let
                a =
                    min c d

                b =
                    max c d

                before =
                    List.take a chars

                lower =
                    List.drop a chars |> List.take 1

                between =
                    List.drop (a + 1) chars |> List.take (b - a - 1)

                upper =
                    List.drop b chars |> List.take 1

                after =
                    List.drop (b + 1) chars
            in
                before ++ upper ++ between ++ lower ++ after

        SwapLetter a b ->
            List.map
                (\c ->
                    if c == a then
                        b
                    else if c == b then
                        a
                    else
                        c
                )
                chars

        RotateBasedOn c ->
            case firstOccurrence c chars of
                Nothing ->
                    chars

                Just j ->
                    let
                        i =
                            if j >= 4 then
                                j + 2
                            else
                                j + 1
                    in
                        process (RotateRight i) chars

        RotateRight j ->
            let
                len =
                    List.length chars

                i =
                    abs <| (len - j) % len
            in
                List.drop i chars ++ List.take i chars

        RotateLeft j ->
            let
                len =
                    List.length chars

                i =
                    j % len
            in
                List.drop i chars ++ List.take i chars

        UnrotateBasedOn c ->
            List.range 0 (List.length chars)
                |> List.map (process << RotateLeft)
                |> List.map ((|>) chars)
                |> List.filter (\newChars -> process (RotateBasedOn c) newChars == chars)
                |> List.head
                |> Maybe.withDefault chars


firstOccurrence : a -> List a -> Maybe Int
firstOccurrence =
    let
        firstOccurrenceMemo : Int -> a -> List a -> Maybe Int
        firstOccurrenceMemo count elem list =
            case list of
                [] ->
                    Nothing

                x :: rest ->
                    if elem == x then
                        Just count
                    else
                        firstOccurrenceMemo (count + 1) elem rest
    in
        firstOccurrenceMemo 0


inverse : Instruction -> Instruction
inverse instruction =
    case instruction of
        MovePosition a b ->
            MovePosition b a

        ReversePositions a b ->
            ReversePositions a b

        SwapPosition a b ->
            SwapPosition a b

        SwapLetter a b ->
            SwapLetter a b

        RotateBasedOn c ->
            UnrotateBasedOn c

        RotateRight i ->
            RotateLeft i

        RotateLeft i ->
            RotateRight i

        UnrotateBasedOn c ->
            RotateBasedOn c


password : List Char
password =
    String.toList "abcdefgh"


input : List Instruction
input =
    [ MovePosition 2 6
    , MovePosition 0 5
    , MovePosition 6 4
    , ReversePositions 3 7
    , MovePosition 1 7
    , SwapPosition 6 3
    , SwapLetter 'g' 'b'
    , SwapPosition 2 3
    , MovePosition 4 3
    , MovePosition 6 3
    , SwapPosition 4 1
    , SwapLetter 'b' 'f'
    , ReversePositions 3 4
    , SwapLetter 'f' 'e'
    , ReversePositions 2 7
    , RotateBasedOn 'h'
    , RotateBasedOn 'a'
    , RotateBasedOn 'e'
    , RotateBasedOn 'h'
    , RotateBasedOn 'c'
    , MovePosition 5 7
    , SwapLetter 'a' 'd'
    , MovePosition 5 6
    , SwapPosition 4 0
    , SwapPosition 4 6
    , RotateLeft 6
    , RotateRight 4
    , RotateRight 5
    , SwapLetter 'f' 'e'
    , SwapPosition 2 7
    , RotateBasedOn 'e'
    , MovePosition 4 5
    , SwapPosition 4 2
    , RotateRight 1
    , SwapLetter 'b' 'f'
    , RotateBasedOn 'b'
    , ReversePositions 3 5
    , MovePosition 3 1
    , RotateBasedOn 'g'
    , SwapLetter 'c' 'e'
    , SwapPosition 7 3
    , MovePosition 0 3
    , RotateRight 6
    , ReversePositions 1 3
    , SwapLetter 'd' 'e'
    , ReversePositions 3 5
    , MovePosition 0 3
    , SwapLetter 'c' 'e'
    , MovePosition 2 7
    , SwapLetter 'g' 'b'
    , RotateRight 0
    , ReversePositions 1 3
    , SwapLetter 'h' 'd'
    , MovePosition 4 0
    , MovePosition 6 3
    , SwapLetter 'a' 'c'
    , ReversePositions 3 6
    , SwapLetter 'h' 'g'
    , MovePosition 7 2
    , RotateBasedOn 'h'
    , SwapLetter 'b' 'h'
    , ReversePositions 2 6
    , MovePosition 6 7
    , RotateBasedOn 'a'
    , RotateRight 7
    , ReversePositions 1 6
    , MovePosition 1 6
    , RotateBasedOn 'g'
    , RotateBasedOn 'd'
    , MovePosition 0 4
    , RotateBasedOn 'e'
    , RotateBasedOn 'd'
    , RotateBasedOn 'a'
    , RotateBasedOn 'a'
    , RotateRight 4
    , RotateBasedOn 'b'
    , ReversePositions 0 4
    , MovePosition 1 7
    , RotateBasedOn 'e'
    , MovePosition 1 7
    , SwapLetter 'f' 'h'
    , MovePosition 5 1
    , RotateBasedOn 'f'
    , ReversePositions 0 1
    , MovePosition 2 4
    , RotateBasedOn 'a'
    , SwapLetter 'b' 'd'
    , MovePosition 6 0
    , SwapLetter 'e' 'b'
    , RotateRight 7
    , MovePosition 2 7
    , RotateLeft 4
    , SwapPosition 6 1
    , MovePosition 3 5
    , RotateRight 7
    , ReversePositions 0 6
    , SwapPosition 2 1
    , ReversePositions 4 6
    , RotateBasedOn 'g'
    , MovePosition 6 4
    ]
