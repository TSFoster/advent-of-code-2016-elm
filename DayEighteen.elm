module DayEighteen exposing (answers)

import Model exposing (..)


answers : List QandA
answers =
    [ QandA (Question "Day 18 part 1") part1
    ]


part1 : Answer
part1 =
    Uncalculated
        (\() ->
            toString <| safeTilesInFloorStarting initialRow 40
        )


type Tile
    = Safe
    | Trap


type alias AdjacentTiles =
    ( Tile, Tile, Tile )


safeTilesInFloorStarting : List Tile -> Int -> Int
safeTilesInFloorStarting initialRow =
    floorStarting initialRow
        >> List.map (List.length << List.filter ((==) Safe))
        >> List.sum


floorStarting : List Tile -> Int -> List (List Tile)
floorStarting initialRow rowCount =
    (initialRow :: getNextRows (rowCount - 1) initialRow)


getNextRows : Int -> List Tile -> List (List Tile)
getNextRows =
    let
        getNextRowsMemo : List (List Tile) -> Int -> List Tile -> List (List Tile)
        getNextRowsMemo memo n row =
            if n <= 0 then
                List.reverse memo
            else
                let
                    nextRow =
                        getNextRow row
                in
                    getNextRowsMemo (nextRow :: memo) (n - 1) nextRow
    in
        getNextRowsMemo []


getNextRow : List Tile -> List Tile
getNextRow =
    adjacentTiles >> List.map getNextTile


getNextTile : AdjacentTiles -> Tile
getNextTile adj =
    case adj of
        ( Trap, Trap, Safe ) ->
            Trap

        ( Safe, Trap, Trap ) ->
            Trap

        ( Trap, Safe, Safe ) ->
            Trap

        ( Safe, Safe, Trap ) ->
            Trap

        _ ->
            Safe


adjacentTiles : List Tile -> List AdjacentTiles
adjacentTiles tiles =
    triples (Safe :: tiles ++ [ Safe ])


triples : List a -> List ( a, a, a )
triples list =
    case list of
        a :: b :: c :: rest ->
            ( a, b, c ) :: triples (b :: c :: rest)

        _ ->
            []


initialRow : List Tile
initialRow =
    strToRow input


strToRow : String -> List Tile
strToRow =
    String.toList >> List.filterMap toTile


toTile : Char -> Maybe Tile
toTile c =
    case c of
        '.' ->
            Just Safe

        '^' ->
            Just Trap

        _ ->
            Nothing


input : String
input =
    ".^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^"
