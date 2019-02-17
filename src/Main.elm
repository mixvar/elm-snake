module Main exposing (main)

import Browser
import Html exposing (Html)



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
    Void


init : Void -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- MESSAGES


type Msg
    = Nop



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( (), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view _ =
    Html.text "Hello World"
