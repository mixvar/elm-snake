module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (..)
import Color exposing (Color)
import Debug
import Html exposing (Html, div, h1)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import List exposing (head, length, take)
import Time


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
    { gameState : GameState
    , arenaDimensions : ArenaDimensions
    , snake : Snake
    , apple : Maybe Position
    }


type GameState
    = Running { speedPerSecond : Int }
    | GameOver


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


directionFromKey : String -> Maybe Direction
directionFromKey key =
    case key of
        "w" ->
            Just Up

        "ArrowUp" ->
            Just Up

        "s" ->
            Just Down

        "ArrowDown" ->
            Just Down

        "a" ->
            Just Left

        "ArrowLeft" ->
            Just Left

        "d" ->
            Just Right

        "ArrowRight" ->
            Just Right

        _ ->
            Nothing


opposite : Direction -> Direction
opposite dir =
    case dir of
        Right ->
            Left

        Left ->
            Right

        Up ->
            Down

        Down ->
            Up


type alias Snake =
    { body : List Position
    , direction : Direction
    , nextDirection : Direction
    }


turn : Direction -> Snake -> Snake
turn direction snake =
    if direction == opposite snake.direction then
        snake

    else
        { snake | nextDirection = direction }


move : ArenaDimensions -> Snake -> Result Void Snake
move dimensions snake =
    -- TODO handle eating apples
    let
        nextHead =
            snake.body
                |> head
                |> Maybe.map (transformPosition snake.nextDirection)
                |> Maybe.map (toArenaPosition dimensions)

        cutSnakeBody =
            take (length snake.body)

        nextBody =
            nextHead
                |> Maybe.map (\head -> head :: snake.body)
                |> Maybe.map cutSnakeBody
                |> Maybe.andThen validateCollisions
    in
    case nextBody of
        Just body ->
            Ok { snake | body = body, direction = snake.nextDirection }

        Nothing ->
            Err ()


validateCollisions : List Position -> Maybe (List Position)
validateCollisions body =
    case body of
        head :: tail ->
            if List.member head tail then
                Nothing

            else
                Just body

        _ ->
            Nothing


transformPosition : Direction -> Position -> Position
transformPosition direction position =
    case direction of
        Right ->
            { position | col = increment position.col }

        Left ->
            { position | col = decrement position.col }

        Up ->
            { position | row = decrement position.row }

        Down ->
            { position | row = increment position.row }


toArenaPosition : ArenaDimensions -> Position -> Position
toArenaPosition dimensions { col, row } =
    let
        bounded limit v =
            if v > limit then
                1

            else if v < 1 then
                limit

            else
                v
    in
    { col = col |> bounded dimensions.cols
    , row = row |> bounded dimensions.rows
    }



-- INIT


init : Void -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { gameState = Running { speedPerSecond = 10 }
    , arenaDimensions = ArenaDimensions 7 80 50
    , snake = initialSnake
    , apple = Just (Position 15 20) -- TODO start with Nothing
    }


initialSnake : Snake
initialSnake =
    { direction = Up
    , nextDirection = Up
    , body = List.range 25 30 |> List.map (\y -> Position 40 y)
    }



-- MESSAGES


type Msg
    = Move
    | KeyPressed String
    | Turn Direction
    | EndGame



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndGame ->
            ( { model | gameState = GameOver }, Cmd.none )

        Move ->
            let
                moveResult =
                    model.snake |> move model.arenaDimensions
            in
            case moveResult of
                Err _ ->
                    model |> update EndGame

                Ok nextSnake ->
                    ( { model | snake = nextSnake }, Cmd.none )

        KeyPressed key ->
            let
                direction =
                    directionFromKey key
            in
            case direction of
                Just dir ->
                    model |> update (Turn dir)

                Nothing ->
                    ( model, Cmd.none )

        Turn direction ->
            ( { model | snake = model.snake |> turn direction }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Running { speedPerSecond } ->
            let
                moveSub =
                    Time.every (1000 / toFloat speedPerSecond) (\_ -> Move)

                keyPressedSub =
                    Browser.Events.onKeyDown keyPressedDecoder
            in
            Sub.batch [ moveSub, keyPressedSub ]

        GameOver ->
            Sub.none


keyPressedDecoder : Decode.Decoder Msg
keyPressedDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyPressed



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
    h1 styles [ Html.text "elm-snake" ]


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
        , renderApple model
        , renderSnake model
        ]


renderBackground : Int -> Int -> Color -> Renderable
renderBackground width height color =
    shapes [ fill color ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderApple : Model -> Renderable
renderApple { apple, arenaDimensions } =
    apple
        |> Maybe.map (renderTile arenaDimensions)
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
        |> shapes [ fill Color.red ]


renderSnake : Model -> Renderable
renderSnake model =
    shapes [ fill (snakeColor model.gameState) ]
        (model.snake.body |> List.map (renderTile model.arenaDimensions))


renderTile : ArenaDimensions -> Position -> Shape
renderTile dims coords =
    renderSquare dims.unitSize (toPoint dims coords)


renderSquare : Int -> Point -> Shape
renderSquare size point =
    rect point (toFloat size) (toFloat size)


snakeColor : GameState -> Color
snakeColor state =
    case state of
        Running _ ->
            Color.green

        GameOver ->
            Color.red
