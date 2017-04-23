module Sandpile
    exposing
        (..)


import Dict exposing (Dict)
import Axis exposing (Lattice, Axis)
import Lattice3


type alias GrainCount =
    Int


type alias Sandpile coordSystemType comparable =
    { lattice : Lattice coordSystemType comparable
    , cells : Dict comparable GrainCount
    }


-- CREATE

sandpile : Lattice cs comparable -> Dict comparable GrainCount -> Sandpile cs comparable
sandpile =
    Sandpile


fromList : Lattice cs comparable -> List (comparable, GrainCount) -> Sandpile cs comparable
fromList lattice =
    sandpile lattice << Dict.fromList


empty : Lattice cs comparable -> Sandpile cs comparable
empty lattice =
    fromList lattice []


-- QUERY

allCoordinates : Sandpile cs comparable -> List comparable
allCoordinates =
    Dict.keys << .cells


get : comparable -> Sandpile cs comparable -> Maybe GrainCount
get coordinates =
    Dict.get coordinates << .cells


getOrZero : comparable -> Sandpile cs comparable -> GrainCount
getOrZero coordinates =
    Maybe.withDefault 0 << get coordinates
    -- TODO
    -- distinguish between an untouched cell vs invalid coordinates


unstableCoordinates : Sandpile cs comparable -> List comparable
unstableCoordinates sandpile =
    let
        isUnstable _ numGrains =
            numGrains >= sandpile.lattice.neighborCount
    in
        Dict.filter isUnstable sandpile.cells
            |> Dict.keys


isStable : Sandpile cs comparable -> Bool
isStable sandpile =
    List.length (unstableCoordinates sandpile) == 0


-- UPDATE

{-
Distribute one grain of sand to each neighbor cell
-}
topple : comparable -> Sandpile cs comparable -> Sandpile cs comparable
topple coordinates sandpile =
    sandpile.lattice.neighbors coordinates
        -- drop a grain at each neighbor location
        |> List.foldr increment sandpile
        -- subtract grains from center position
        |> subtract sandpile.lattice.neighborCount coordinates


{-
Set the number of grains at a particular cell
-}
set : Int -> comparable -> Sandpile cs comparable -> Sandpile cs comparable
set n coordinates sandpile =
    { sandpile
        | cells = Dict.insert coordinates n sandpile.cells
    }

{-
Add n grains at a particular cell
-}
add : Int -> comparable -> Sandpile cs comparable -> Sandpile cs comparable
add n =
    update ((+) n)


{-
Add one grain at a particular cell
-}
increment : comparable -> Sandpile cs comparable -> Sandpile cs comparable
increment =
    add 1


{-
Subtract some grains from a particular cell
-}
subtract : Int -> comparable -> Sandpile cs comparable -> Sandpile cs comparable
subtract n =
    update (flip (-) n)


{-
Update the number of grains at a particular cell
-}
update : (Int -> Int) -> comparable -> Sandpile cs comparable -> Sandpile cs comparable
update updateCell coordinates sandpile =
    -- TODO if the location is not valid, return unchanged
    let
        newGrainCount =
            getOrZero coordinates sandpile |> updateCell
    in
        set newGrainCount coordinates sandpile


changeMany :
    (comparable -> Sandpile cs comparable -> Sandpile cs comparable)
    -> List comparable
    -> Sandpile cs comparable
    -> Sandpile cs comparable
changeMany changeSandpile locations sandpile =
    List.foldr changeSandpile sandpile locations


{-
Topple all the unstable cells in a sandpile
-}
toppleStep : Sandpile cs comparable -> Sandpile cs comparable
toppleStep sandpile =
    changeMany topple (unstableCoordinates sandpile) sandpile


{-
-}
toppleSettle : Sandpile cs comparable -> Sandpile cs comparable
toppleSettle sandpile =
    if isStable sandpile then
        sandpile
    else
        toppleStep sandpile
            |> toppleSettle
