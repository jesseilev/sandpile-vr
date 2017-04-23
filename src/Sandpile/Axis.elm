module Axis exposing (..)


type Axis
    = Infinite
    | Bounded Int
    | Wrap Int


infinite : Axis
infinite =
    Infinite


bounded : Int -> Axis
bounded =
    Bounded


wrap : Int -> Axis
wrap =
    Wrap


---


previous : Axis -> Int -> Maybe Int
previous axis =
    makeValidSoft axis << (flip (-) 1)


next : Axis -> Int -> Maybe Int
next axis =
    makeValidSoft axis << ((+) 1)


isValid : Axis -> Int -> Bool
isValid axis n =
    makeValidHard axis n == n


canBeMadeValid : Axis -> Int -> Bool
canBeMadeValid axis n =
    case axis of
        Bounded _ ->
            isValid axis n

        _ ->
            True


makeValidSoft : Axis -> Int -> Maybe Int
makeValidSoft axis n =
    if canBeMadeValid axis n then
        Just (makeValidHard axis n)
    else
        Nothing


makeValidHard : Axis -> Int -> Int
makeValidHard axis n =
    case axis of
        Infinite ->
            n

        Wrap size ->
            n % size

        Bounded size ->
            n |> clamp 0 (size - 1)


---


-- TODO move this?
type alias Lattice coordSystemType comparable =
    { neighbors : comparable -> List comparable
    , coordSystem : coordSystemType
    , neighborCount : Int
    }
