module DayEleven exposing (answers)

import Model exposing (..)
import Set exposing (Set)


answers : List QandA
answers =
    [ QandA (Question "Day 11 part 1") part1
    ]


type alias Building =
    { below : List Items
    , current : Items
    , above : List Items
    }


type alias Items =
    List Item


type Item
    = Generator Element
    | Microchip Element


type Element
    = Strontium
    | Plutonium
    | Thulium
    | Ruthenium
    | Curium


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            movesFromSolution start
                |> Maybe.map toString
                |> Maybe.withDefault "No solution found"
        )


movesFromSolution : Building -> Maybe Int
movesFromSolution building =
    if isSolved building then
        Just 0
    else
        stepTillSolutionFound (Set.singleton <| hash building) [ ( 1, building ) ]


stepTillSolutionFound : Set String -> List ( Int, Building ) -> Maybe Int
stepTillSolutionFound seen states =
    case states of
        [] ->
            Nothing

        ( moves, building ) :: rest ->
            let
                combos =
                    possibleElevatorCombinations building

                unseen =
                    hash >> (flip Set.member seen) >> not

                nextMoves =
                    List.filterMap moveUp combos
                        ++ List.filterMap moveDown combos
                        |> List.filter unseen
            in
                if List.any isSolved nextMoves then
                    Just moves
                else
                    stepTillSolutionFound
                        (addMoves seen nextMoves)
                        (rest ++ (List.map ((,) (moves + 1)) nextMoves))


addMoves : Set String -> List Building -> Set String
addMoves =
    List.foldl (hash >> Set.insert)


hash : Building -> String
hash { below, current, above } =
    let
        floors =
            below
                ++ current
                :: above
                |> List.indexedMap (,)
                |> List.map (Tuple.mapFirst ((+) 1))
                |> List.concatMap (Tuple.mapFirst (,) >> uncurry List.map)

        getPairLoc : Item -> Int
        getPairLoc item =
            let
                ( i, b ) =
                    floors
                        |> List.filter (Tuple.second >> isPair item)
                        |> List.map (Tuple.mapSecond isGenerator)
                        |> List.head
                        |> Maybe.withDefault ( 0, True )
            in
                if b then
                    i
                else
                    -i
    in
        floors
            |> List.map (Tuple.second >> getPairLoc)
            |> List.sort
            |> List.map toString
            |> String.join ""
            |> (++) (toString <| List.length below)


moveDown : ( List Item, Building ) -> Maybe Building
moveDown ( items, { below, current, above } ) =
    case below of
        [] ->
            Nothing

        next :: rest ->
            let
                newCurrent =
                    next ++ items
            in
                if isSafe newCurrent then
                    Just
                        { below = rest
                        , current = newCurrent
                        , above = current :: above
                        }
                else
                    Nothing


moveUp : ( List Item, Building ) -> Maybe Building
moveUp ( items, { below, current, above } ) =
    case above of
        [] ->
            Nothing

        next :: rest ->
            let
                newCurrent =
                    next ++ items
            in
                if isSafe newCurrent then
                    Just
                        { below = current :: below
                        , current = newCurrent
                        , above = rest
                        }
                else
                    Nothing


possibleElevatorCombinations : Building -> List ( List Item, Building )
possibleElevatorCombinations ({ current } as building) =
    partitionAllSingletons current
        ++ partitionAllPairs current
        |> List.filter (Tuple.first >> isSafe)
        |> List.filter (Tuple.second >> isSafe)
        |> List.map (Tuple.mapSecond (\floor -> { building | current = floor }))


partitionAllSingletons : List a -> List ( List a, List a )
partitionAllSingletons =
    let
        allSingletonsMemo memo prev list =
            case list of
                [] ->
                    memo

                x :: xs ->
                    allSingletonsMemo (( [ x ], xs ++ prev ) :: memo) (x :: prev) xs
    in
        allSingletonsMemo [] []


partitionAllPairs : List a -> List ( List a, List a )
partitionAllPairs =
    let
        allPairsMemo memo prev list =
            case list of
                [] ->
                    memo

                x :: xs ->
                    let
                        newMemo =
                            xs
                                |> partitionAllSingletons
                                |> List.map (Tuple.mapFirst ((::) x))
                                |> List.map (Tuple.mapSecond ((++) prev))
                    in
                        allPairsMemo (memo ++ newMemo) (x :: prev) xs
    in
        allPairsMemo [] []


isSafe : Items -> Bool
isSafe items =
    let
        generatorCount =
            items
                |> List.filter isGenerator
                |> List.length

        unsafeMicrochipCount =
            items
                |> List.filter isMicrochip
                |> List.map (Generator << elem)
                |> List.filter (notIn items)
                |> List.length
    in
        generatorCount * unsafeMicrochipCount == 0


isSolved : Building -> Bool
isSolved { below, above } =
    List.isEmpty above && List.all List.isEmpty below


isGenerator : Item -> Bool
isGenerator item =
    case item of
        Generator _ ->
            True

        _ ->
            False


isMicrochip : Item -> Bool
isMicrochip item =
    case item of
        Microchip _ ->
            True

        _ ->
            False


isPair : Item -> Item -> Bool
isPair first second =
    isGenerator first == isMicrochip second && elem first == elem second


elem : Item -> Element
elem item =
    case item of
        Generator x ->
            x

        Microchip x ->
            x


notIn : List a -> a -> Bool
notIn list item =
    not <| List.member item list


start : Building
start =
    let
        first =
            [ Generator Strontium, Generator Plutonium, Microchip Strontium, Microchip Plutonium ]

        second =
            [ Generator Thulium, Generator Ruthenium, Generator Curium, Microchip Ruthenium, Microchip Curium ]

        third =
            [ Microchip Thulium ]

        fourth =
            []
    in
        { below = [], current = first, above = [ second, third, fourth ] }
