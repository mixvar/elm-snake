module Main exposing (main)

import Browser
import Canvas exposing (..)
import Color
import Html exposing (Html, div, text)
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
    { arenaDimensions = ArenaDimensions 45 24 12 }



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
            [ style "text-align" "center", style "font-size" "36px" ]
    in
    div styles [ text "elm-snake" ]



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
            [ style "border" "1px solid red"
            , style "position" "absolute"
            , style "top" "50%"
            , style "left" "50%"
            , style "transform" "translate(-50%, -50%)"
            ]
    in
    Canvas.toHtml ( width, height )
        canvasStyles
        [ shapes [ fill Color.red ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
        ]
