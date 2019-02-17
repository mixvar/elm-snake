module Main exposing (main)

import Browser
import Canvas exposing (..)
import Color exposing (Color)
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



-- FIXME


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
            [ style "border" "2px solid white"
            , style "position" "absolute"
            , style "top" "50%"
            , style "left" "50%"
            , style "transform" "translate(-50%, -50%)"
            ]
    in
    Canvas.toHtml ( width, height )
        canvasStyles
        [ renderBackground width height Color.black
        , renderSquare pixelSize ( 0, 0 ) Color.green
        ]


renderBackground : Int -> Int -> Color -> Renderable
renderBackground width height color =
    shapes [ fill color ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderSquare : Int -> Point -> Color -> Renderable
renderSquare size ( x, y ) color =
    shapes [ fill color ] [ rect ( x, y ) (toFloat size) (toFloat size) ]
