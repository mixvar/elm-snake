module Main exposing (main)

import Browser
import Canvas exposing (..)
import Color exposing (Color)
import Debug
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)



-- SETUP


type alias Void =
    ()


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { arenaDimensions : ArenaDimensions }


type alias ArenaDimensions =
    { pixelSize : Int, cols : Int, rows : Int }


type alias Position =
    { col : Int, row : Int }



-- TODO validate if Position is ok?


toPoint : ArenaDimensions -> Position -> Point
toPoint { pixelSize } { col, row } =
    let
        x =
            toFloat <| (col - 1) * pixelSize

        y =
            toFloat <| (row - 1) * pixelSize
    in
    ( x, y )


init : Void -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { arenaDimensions = ArenaDimensions 20 26 16 }



-- MESSAGES


type Msg
    = Nop



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ titleView
        , gameArenaView model
        ]


titleView =
    let
        styles =
            [ style "text-align" "center", style "font-size" "40px" ]
    in
    h1 styles [ text "elm-snake" ]


gameArenaView : Model -> Html Msg
gameArenaView model =
    let
        { pixelSize, cols, rows } =
            model.arenaDimensions

        width =
            pixelSize * cols

        height =
            pixelSize * rows

        canvasStyles =
            [ style "position" "absolute"
            , style "top" "50%"
            , style "left" "50%"
            , style "transform" "translate(-50%, -50%)"
            ]
    in
    Canvas.toHtml ( width, height )
        canvasStyles
        [ renderBackground width height Color.black
        , renderTile model.arenaDimensions Color.red (Position 1 1)
        , renderTile model.arenaDimensions Color.green (Position (cols // 2) (rows // 2))
        , renderTile model.arenaDimensions Color.blue (Position cols rows)
        ]


renderBackground : Int -> Int -> Color -> Renderable
renderBackground width height color =
    shapes [ fill color ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderTile : ArenaDimensions -> Color -> Position -> Renderable
renderTile dims color coords =
    renderSquare dims.pixelSize (toPoint dims coords) color


renderSquare : Int -> Point -> Color -> Renderable
renderSquare size point color =
    shapes [ fill color ] [ rect point (toFloat size) (toFloat size) ]
