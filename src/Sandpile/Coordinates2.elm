module Coordinates2 exposing (..)


import Axis exposing (Axis)


type alias Coordinates =
    (Int, Int)


type System
    = System Axis Axis



-- CREATE COORDINATES

coordinates : Int -> Int -> Coordinates
coordinates =
    (,)


origin : Coordinates
origin =
    coordinates 0 0


-- MISC

toFloats : Coordinates -> (Float, Float)
toFloats (x, y) =
    (toFloat x, toFloat y)


-- MODIFY COORDINATES

scale : Int -> Coordinates -> Coordinates
scale n =
    Tuple.mapSecond ((*) n) << Tuple.mapFirst ((*) n)


previousX : Axis -> Coordinates -> Maybe Coordinates
previousX =
    updateCoord Axis.previous Tuple.first insertFirst


previousY : Axis -> Coordinates -> Maybe Coordinates
previousY =
    updateCoord Axis.previous Tuple.second insertSecond


nextX : Axis -> Coordinates -> Maybe Coordinates
nextX =
    updateCoord Axis.next Tuple.first insertFirst


nextY : Axis -> Coordinates -> Maybe Coordinates
nextY =
    updateCoord Axis.next Tuple.second insertSecond


updateCoord axisFunc tupleGet tupleInsert axis coordinates =
    axisFunc axis (tupleGet coordinates)
        |> Maybe.map (flip tupleInsert coordinates)


-- CREATE COORDINATE SYSTEM


system : Axis -> Axis -> System
system =
    System


plane : System
plane =
    system Axis.infinite Axis.infinite


rect : Int -> Int -> System
rect width height =
    system (Axis.bounded width) (Axis.bounded height)


donut : Int -> Int -> System
donut width height =
    system (Axis.wrap width) (Axis.wrap height)


tubeV : Int -> System
tubeV width =
    system (Axis.wrap width) Axis.infinite


tubeH : Int -> System
tubeH height =
    system Axis.infinite (Axis.wrap height)


cylinderV : Int -> Int -> System
cylinderV width height =
    system (Axis.wrap width) (Axis.bounded height)


cylinderH : Int -> Int -> System
cylinderH width height =
    system (Axis.bounded width) (Axis.wrap height)


stripV : Int -> System
stripV width =
    system (Axis.bounded width) Axis.infinite


stripH : Int -> System
stripH height =
    system Axis.infinite (Axis.bounded height)



-- TUPLE UTILS

insertFirst : a -> (a, b) -> (a, b)
insertFirst p (_, q) =
    (p, q)


insertSecond : b -> (a, b) -> (a, b)
insertSecond q (p, _) =
    (p, q)
