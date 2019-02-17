module Main exposing (main)

import Browser
import Canvas exposing (..)
import Color exposing (Color)
import Debug
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)
import List exposing (head, length, take)


increment val =
    val + 1


decrement val =
    val - 1



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
    { unitSize : Int, cols : Int, rows : Int }


type alias Position =
    { col : Int, row : Int }


toPoint : ArenaDimensions -> Position -> Point
toPoint { unitSize } { col, row } =
    let
        toPixels =
            decrement >> (*) unitSize >> toFloat
    in
    ( toPixels col, toPixels row )


type Direction
    = Right
    | Left
    | Up
    | Down


type alias Snake =
    { body : List Position, direction : Direction }


changeDirection : Direction -> Snake -> Snake
changeDirection direction snake =
    { snake | direction = direction }


move : ArenaDimensions -> Snake -> Result Snake Snake
move dimensions snake =
    -- TODO handle eating apples
    -- TODO handle snake collision
    let
        nextHead =
            snake.body
                |> head
                |> Maybe.map (transformPosition snake.direction)
                |> Maybe.map (toArenaPosition dimensions)

        body =
            case nextHead of
                Just head ->
                    (head :: snake.body)
                        |> take (length snake.body)

                Nothing ->
                    snake.body

        resultingSnake =
            { snake | body = body }
    in
    case nextHead of
        Just _ ->
            Ok resultingSnake

        Nothing ->
            Err resultingSnake


transformPosition : Direction -> Position -> Position
transformPosition direction position =
    case direction of
        Right ->
            { position | col = increment position.col }

        Left ->
            { position | col = decrement position.col }

        Up ->
            { position | col = increment position.row }

        Down ->
            { position | col = decrement position.row }


toArenaPosition : ArenaDimensions -> Position -> Position
toArenaPosition { cols, rows } position =
    let
        boundedCol =
            position.col

        boundedRow =
            position.row
    in
    { col = boundedCol, row = boundedRow }


init : Void -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { arenaDimensions = ArenaDimensions 10 56 34 }



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
        { unitSize, cols, rows } =
            model.arenaDimensions

        width =
            unitSize * cols

        height =
            unitSize * rows

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
    renderSquare dims.unitSize (toPoint dims coords) color


renderSquare : Int -> Point -> Color -> Renderable
renderSquare size point color =
    shapes [ fill color ] [ rect point (toFloat size) (toFloat size) ]
