module Lattice3 exposing (..)

import Axis exposing (Axis, Lattice)
import Coordinates3 as Coord exposing (Coordinates)
import Maybe.Extra as MaybeEx


neighbors : Coord.System -> Coordinates -> List Coordinates
neighbors (Coord.System xAxis yAxis zAxis) coordinates =
    List.map ((|>) coordinates)
        [ Coord.previousX xAxis
        , Coord.previousY yAxis
        , Coord.previousZ zAxis
        , Coord.nextX xAxis
        , Coord.nextY yAxis
        , Coord.nextZ zAxis
        ]
        |> MaybeEx.values


lattice : Coord.System -> Lattice Coord.System Coordinates
lattice coordSystem =
    { neighbors = neighbors coordSystem
    , neighborCount = 6
    , coordSystem = coordSystem
    }


space : Lattice Coord.System Coordinates
space =
    lattice Coord.space
