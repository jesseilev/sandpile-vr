

import Sandpile exposing (Sandpile)
import Coordinates2 as Coord2 exposing (Coordinates)
import Lattice2
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as HtmlAttr
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import Time


type alias Model =
    { sandpile : Sandpile Coord2.System Coordinates
    , playback : Playback
    , dripLocations : List Coordinates
    }


type Playback
    = Play
    | Pause


init : (Model, Cmd Msg)
init =
    { sandpile = Sandpile.empty (Lattice2.rect 21 21)
    , playback = Pause
    , dripLocations =
        [ Coord2.coordinates 6 6
        , Coord2.coordinates 14 6
        , Coord2.coordinates 10 14
        ]
    }
        ! []


type Msg
    = DropGrain
    | ToppleStep
    | ToppleSettle
    | TogglePlayback


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DropGrain ->
            updateSandpile
                (Sandpile.changeMany Sandpile.increment model.dripLocations)
                model ! []

        ToppleStep ->
            updateSandpile Sandpile.toppleStep model ! []

        ToppleSettle ->
            updateSandpile Sandpile.toppleSettle model ! []

        TogglePlayback ->
            { model | playback = if model.playback == Play then Pause else Play } ! []


updateSandpile updateS model =
    { model | sandpile = updateS model.sandpile }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playback == Play then
        Sub.batch
            [ Time.every (0.25 * Time.second) (\_ -> DropGrain)
            , Time.every (50 * Time.millisecond) (\_ -> ToppleStep)
            ]
    else
        Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ viewSandpile model
        , viewControls model
        ]


viewSandpile : Model -> Html a
viewSandpile model =
    svg [ SvgAttr.width "600", SvgAttr.height "600", SvgAttr.viewBox "0 0 600 600" ]
        <| List.map (viewCell model) (Sandpile.allCoordinates model.sandpile)


viewCell : Model -> Coordinates -> Svg a
viewCell model coordinates =
    let
        grainCount =
            Sandpile.get coordinates model.sandpile
                |> Maybe.withDefault 0

        center : (Float, Float)
        center =
            coordinates
                |> Coord2.scale 20
                |> Coord2.toFloats
    in
        Svg.circle2d
            [ SvgAttr.fill (colorFromGrainCount grainCount)
            ]
            (Circle2d { centerPoint = Point2d center, radius = 10 })


colorFromGrainCount grainCount =
    case grainCount of
        0 -> "white"
        1 -> "yellow"
        2 -> "orange"
        3 -> "blue"
        _ -> "black"


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
        ]


viewButton text onClickMsg isDisabled =
    Html.button
        [ Events.onClick onClickMsg, HtmlAttr.disabled isDisabled ]
        [ Html.text text ]


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



{-
TODO

----------Short Term----------------

- 3d / vr

- flexbox scaffolding ui


- click to drop a grain / start a drip
- pan / zoom

- keep track of unstables

- tests

----------Long Term--------------

- game!

- save

- performance at bigger scale
    - WebGL?

- more controls
    - colors
    - coordinate system
    - speed

- nicer controls ui
    - glyphs
    - tooltips

- animations
    - topple
    - settle

- undo / go backwards

-}
