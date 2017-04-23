port module Main exposing (..)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes as HtmlAttr
import Color exposing (..)
import Time

import AFrame exposing (..)
import AFrame.Primitives.Attributes exposing (..)
import AFrame.Primitives exposing (..)
import AFrame.Primitives.Camera exposing (..)
import AFrame.Primitives.Cursor as Cursor
import AFrame.Animations as Ani
import List.Extra as ListEx
import Maybe.Extra as MaybeEx

import Sandpile exposing (Sandpile)
import Coordinates2 as Coord2
import Coordinates3 as Coord3 exposing (Coordinates)
import Lattice2
import Lattice3


-- PORTS


-- port for telling Aframe to enter fullscreen VR mode
port enterVr : () ->  Cmd msgType



-- MODEL


type alias Model =
    { sandpile : Sandpile Coord3.System Coordinates
    , playback : Playback
    , dripLocations : List Coordinates
    , menuLocation : Maybe Coordinates
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
                        (coord, (x+y+z) % 1)
                    )
            )
            |> Sandpile.add 1 (Coord3.coordinates 0 0 0)
    , playback = Pause
    , dripLocations =
        [ Coord3.coordinates 0 0 0
        ]
    , menuLocation = Nothing
    }
        ! []



-- UPDATE


type Msg
    = DropGrain
    | ToppleStep
    | ToppleSettle
    | TogglePlayback
    | ClickEnterVr
    | ClickCell Coord3.Coordinates


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
            { model |
                playback =
                    if model.playback == Pause then Play else Pause
            } ! []

        ClickEnterVr ->
            (model, enterVr ())

        ClickCell coords ->
            -- updateSandpile (Sandpile.increment coords) model ! []
            { model | menuLocation = Just coords } ! []


updateSandpile updateS model =
    { model | sandpile = updateS model.sandpile }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case (model.playback, Sandpile.isStable model.sandpile) of
        (Play, True) ->
            Time.every (250 * Time.millisecond ) (\_ -> DropGrain)
        (_, False) ->
            Time.every (250 * Time.millisecond) (\_ -> ToppleStep)
        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ viewControls model
        , viewSandpile model
        ]
    -- Html.div []
    --     [ --viewControls model
    --     viewSandpile model
    --     ]


viewSandpile model =
    let
        cells =
            List.map (viewCell model) (Sandpile.allCoordinates model.sandpile)
    in
        scene
            [ vrModeUi False ]
            ( cells ++
                [ viewCamera model
                , viewMenu model
                , sky [ color skyColor ] []
                ]
            )


viewMenu model =
    case model.menuLocation of
        Nothing ->
            entity [] []

        Just cellCoords ->
            let (x,y,z) = cellCoords |> toSphereCoords in
            plane
                [ position x y z
                , width 1
                , height 1
                , color yellow
                ] []


viewCamera model =
    camera
        [ position 12 12 12 ]
        [ Cursor.cursor
            [ Cursor.fuse True
            -- , HtmlAttr.attribute "fuse-timeout" "1500"
            , position 0 0 -1
            , HtmlAttr.attribute "max-distance" "2"
            , color white -- (rgba 0 0 0 0)
            -- , HtmlAttr.attribute "geometry" "radiusInner: 0; radiusOuter: 0"
            ]
            [ Ani.animation
                [ HtmlAttr.attribute "begin" "cursor-fusing"
                , Ani.attribute_ "scale"
                , Ani.from "1 1 1"
                , Ani.to "0 0 0"
                , Ani.dur 1500
                , Ani.fill "both"
                ] []
            , Ani.animation
                [ HtmlAttr.attribute "begin" "click"
                , Ani.attribute_ "scale"
                , Ani.from "0 0 0"
                , Ani.to "4 4 4"
                , Ani.dur 250
                , Ani.fill "none"
                ] []
            --, cylinder [ radius 0.005, height 0.02, color yellow ] []
            ]
        ]


viewCell : Model -> Coordinates -> Html Msg
viewCell model coordinates =
    let
        grainCount =
            Sandpile.get coordinates model.sandpile
                |> Maybe.withDefault 0

        (x,y,z) =
            coordinates |> toSphereCoords

        wire rotationAttr =
            cylinder [ radius 0.05, height sphereSpacing, color grey, rotationAttr ] []
    in
        sphere
            [ radius sphereRadius
            , position x y z
            , color (colorFromGrainCount grainCount)
            , opacity <| opacityFromGrainCount
                model.sandpile.lattice.neighborCount grainCount
            , Events.onClick (ClickCell coordinates)
            ]
            []
            -- [ wire (rotation 0 0 0)
            -- , wire (rotation 90 0 0)
            -- , wire (rotation 0 0 90)
            -- ]


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
        , viewButton "Enter VR!" ClickEnterVr False
        ]


viewButton text onClickMsg isDisabled =
    Html.button
        [ Events.onClick onClickMsg, HtmlAttr.disabled isDisabled ]
        [ Html.text text ]



-- VIEW HELPERS


skyColor =
    Color.greyscale 0.75


sphereSpacing =
    20


sphereRadius =
    1


toSphereCoords (x,y,z) =
    let scaleVal v = toFloat v * sphereRadius * sphereSpacing / 2 in
    (scaleVal x, scaleVal y, scaleVal z)



colorFromGrainCount grainCount =
    [ white, yellow, orange, red, purple, blue, darkCharcoal ]
        |> List.reverse
        |> ListEx.getAt grainCount
        |> Maybe.withDefault white


opacityFromGrainCount neighborCount grainCount =
    (toFloat grainCount + 1) / (toFloat neighborCount + 1)



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
