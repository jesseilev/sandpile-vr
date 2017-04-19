module Main exposing (..)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes as HtmlAttr
import Color exposing (..)
import Time

import AFrame exposing (..)
import AFrame.Primitives.Attributes exposing (..)
import AFrame.Primitives exposing (..)
import AFrame.Primitives.Camera exposing (..)
import List.Extra as ListEx

import Sandpile exposing (Sandpile)
import Coordinates2 as Coord2
import Coordinates3 as Coord3 exposing (Coordinates)
import Lattice2
import Lattice3


type alias Model =
    { sandpile : Sandpile Coord3.System Coordinates
    , playback : Playback
    , dripLocations : List Coordinates
    , clicks : Int
    }


type Playback
    = Play
    | Pause


init : (Model, Cmd Msg)
init =
    { sandpile =
        Sandpile.fromList (Lattice3.space)
            ( Coord3.allCoordsInRange -4 4 -4 4 -4 4
                |> List.map
                    (\((x,y,z) as coord) ->
                        (coord, (x+y+z) % 6)
                    )
            )
            |> Sandpile.add 1 (Coord3.coordinates 0 0 0)
    , playback = Pause
    , dripLocations =
        [ Coord3.coordinates 0 0 0
        ]
    , clicks = 0
    }
        ! []


type Msg
    = DropGrain
    | ToppleStep
    | ToppleSettle
    | TogglePlayback
    | Click


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DropGrain ->
            updateSandpile
                (Sandpile.changeMany (Sandpile.add 1) model.dripLocations)
                model ! []

        ToppleStep ->
            updateSandpile Sandpile.toppleStep model ! []

        ToppleSettle ->
            updateSandpile Sandpile.toppleSettle model ! []

        TogglePlayback ->
            { model | playback = if model.playback == Play then Pause else Play } ! []

        Click ->
            { model | clicks = model.clicks + 1 } ! []


updateSandpile updateS model =
    { model | sandpile = updateS model.sandpile }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playback == Play then
        Sub.batch
            [ Time.every (0.3 * Time.second) (\_ -> DropGrain)
            , Time.every (50 * Time.millisecond) (\_ -> ToppleStep)
            ]
    else
        Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ --viewControls model
        viewSandpile model
        ]


viewSandpile model =
    let
        cells =
            List.map (viewCell model) (Sandpile.allCoordinates model.sandpile)
    in
        scene
            [ vrModeUi False ]
            ( cells ++
                [ camera [ position 12 12 12 ] []
                , sky [ color skyColor ] []
                ]
            )


skyColor =
    Color.greyscale 0.75


viewCell : Model -> Coordinates -> Html Msg
viewCell model coordinates =
    let
        grainCount =
            Sandpile.get coordinates model.sandpile
                |> Maybe.withDefault 0

        r =
            1.0

        spacing =
            20.0

        scaleCoord c =
            c * r * spacing / 2.0

        (x,y,z) =
            coordinates
                |> Coord3.toFloats
                |> (\(x,y,z) -> (scaleCoord x, scaleCoord y, scaleCoord z))

        wire rotationAttr =
            cylinder [ radius 0.05, height spacing, color grey, rotationAttr ] []
    in
        sphere
            [ radius r
            , position x y z
            , color (colorFromGrainCount grainCount)
            , opacity <| opacityFromGrainCount
                model.sandpile.lattice.neighborCount grainCount
            ]
            []
            -- [ wire (rotation 0 0 0)
            -- , wire (rotation 90 0 0)
            -- , wire (rotation 0 0 90)
            -- ]


colorFromGrainCount grainCount =
    [ white, yellow, orange, red, purple, blue, darkCharcoal ]
        |> List.reverse
        |> ListEx.getAt grainCount
        |> Maybe.withDefault white


opacityFromGrainCount neighborCount grainCount =
    (toFloat grainCount + 1) / (toFloat neighborCount + 1)


viewControls : Model -> Html Msg
viewControls model =
    Html.div []
        [ Html.button
            [ Events.onClick TogglePlayback ]
            [ Html.text <| if model.playback == Play then "Pause" else "Play" ]
        , viewButton "Drop" DropGrain (model.playback == Play)
        , viewButton "Step" ToppleStep
            (model.playback == Play || Sandpile.isStable model.sandpile)
        , viewButton "Settle" ToppleSettle
            (model.playback == Play || Sandpile.isStable model.sandpile)
        , viewButton "Click!" Click False
        ]


viewButton text onClickMsg isDisabled =
    Html.button
        [ Events.onClick onClickMsg, HtmlAttr.disabled isDisabled ]
        [ Html.text text ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
