module Coordinates3 exposing (..)


import Axis exposing (Axis)
import Tuple3
import List.Extra as ListEx


type alias Coordinates =
    (Int, Int, Int)


type System
    = System Axis Axis Axis



-- CREATE COORDINATES

coordinates : Int -> Int -> Int -> Coordinates
coordinates =
    (,,)


toFloats (x,y,z) =
    (toFloat x, toFloat y, toFloat z)


previousX : Axis -> Coordinates -> Maybe Coordinates
previousX =
    updateCoord Axis.previous Tuple3.first insertFirst


previousY : Axis -> Coordinates -> Maybe Coordinates
previousY =
    updateCoord Axis.previous Tuple3.second insertSecond


previousZ : Axis -> Coordinates -> Maybe Coordinates
previousZ =
    updateCoord Axis.previous Tuple3.third insertThird


nextX : Axis -> Coordinates -> Maybe Coordinates
nextX =
    updateCoord Axis.next Tuple3.first insertFirst


nextY : Axis -> Coordinates -> Maybe Coordinates
nextY =
    updateCoord Axis.next Tuple3.second insertSecond


nextZ : Axis -> Coordinates -> Maybe Coordinates
nextZ =
    updateCoord Axis.next Tuple3.third insertThird


updateCoord axisFunc tupleGet tupleInsert axis coordinates =
    axisFunc axis (tupleGet coordinates)
        |> Maybe.map (flip tupleInsert coordinates)


allCoordsInRange xmin xmax ymin ymax zmin zmax =
    ListEx.lift3 (,,)
        (List.range xmin xmax)
        (List.range ymin ymax)
        (List.range zmin zmax)


-- CREATE COORDINATE SYSTEM


system : Axis -> Axis -> Axis -> System
system =
    System


space : System
space =
    system Axis.infinite Axis.infinite Axis.infinite


box : Int -> Int -> Int -> System
box width height depth =
    system (Axis.bounded width) (Axis.bounded height) (Axis.bounded depth)



-- TUPLE UTILS

insertFirst : a -> (a, b, c) -> (a, b, c)
insertFirst p (_, q, r) =
    (p, q, r)


insertSecond : b -> (a, b, c) -> (a, b, c)
insertSecond q (p, _, r) =
    (p, q, r)


insertThird : c -> (a, b, c) -> (a, b, c)
insertThird r (p, q, _) =
    (p, q, r)
