module Lattice2 exposing (..)

import Axis exposing ( Lattice, Axis )
import Coordinates2 as Coord exposing (Coordinates)
import Maybe.Extra as MaybeEx



neighbors : Coord.System -> Coordinates -> List Coordinates
neighbors (Coord.System xAxis yAxis) coordinates =
    List.map ((|>) coordinates)
        [ Coord.previousX xAxis
        , Coord.previousY yAxis
        , Coord.nextX xAxis
        , Coord.nextY yAxis
        ]
        |> MaybeEx.values


--


lattice : Coord.System -> Lattice Coord.System Coordinates
lattice coordSystem =
    { neighbors = neighbors coordSystem
    , neighborCount = 4
    , coordSystem = coordSystem
    }


plane : Lattice Coord.System Coordinates
plane =
    lattice Coord.plane


rect width =
    lattice << Coord.rect width


donut width =
    lattice << Coord.donut width


tubeV =
    lattice << Coord.tubeV 
