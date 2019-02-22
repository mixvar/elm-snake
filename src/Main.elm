module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (..)
import Color exposing (Color)
import Html exposing (Html, div, h1)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import List exposing (head, length, take)
import Random
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
    , score : Int
    , arenaDimensions : ArenaDimensions
    , snake : Snake
    , apple : Apple
    }


type GameState
    = Running
    | GameOver


type alias ArenaDimensions =
    { unitSize : Int, cols : Int, rows : Int }


type alias Position =
    { col : Int, row : Int }


type alias Apple =
    Position


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
    , turnQueue : List Direction
    }


turn : Direction -> Snake -> Snake
turn direction snake =
    { snake | turnQueue = List.append snake.turnQueue [ direction ] }


getNextDirection : Snake -> ( Direction, List Direction )
getNextDirection snake =
    case List.head snake.turnQueue of
        Just nextDir ->
            if nextDir == opposite snake.direction || nextDir == snake.direction then
                getNextDirection { snake | turnQueue = List.drop 1 snake.turnQueue }

            else
                ( nextDir, List.drop 1 snake.turnQueue )

        Nothing ->
            ( snake.direction, [] )


type MoveResult
    = Moved Snake
    | MovedAndFed Snake
    | Collision


move : ArenaDimensions -> Apple -> Snake -> MoveResult
move dimensions apple snake =
    let
        ( direction, turnQueue ) =
            getNextDirection snake

        maybeNextHead =
            snake.body
                |> head
                |> Maybe.map (transformPosition direction)
                |> Maybe.map (toArenaPosition dimensions)

        snakeFed =
            maybeNextHead
                |> Maybe.map ((==) apple)
                |> Maybe.withDefault False

        adjustBody =
            if snakeFed then
                identity

            else
                take (length snake.body)

        maybeNextSnake =
            maybeNextHead
                |> Maybe.map (\head -> head :: snake.body)
                |> Maybe.map adjustBody
                |> Maybe.andThen validateCollisions
                |> Maybe.map
                    (\body ->
                        { snake | body = body, direction = direction, turnQueue = turnQueue }
                    )
    in
    case ( maybeNextSnake, snakeFed ) of
        ( Just nextSnake, True ) ->
            MovedAndFed nextSnake

        ( Just nextSnake, False ) ->
            Moved nextSnake

        ( Nothing, _ ) ->
            Collision


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
    { gameState = Running
    , score = 1
    , arenaDimensions = ArenaDimensions 17 36 20
    , snake = initialSnake
    , apple = Position 18 5
    }


initialSnake : Snake
initialSnake =
    { direction = Up
    , turnQueue = []
    , body = List.range 15 20 |> List.map (\y -> Position 18 y)
    }



-- MESSAGES


type Msg
    = Move
    | NewApple Apple
    | EatApple
    | TimePenalty
    | KeyPressed String
    | Turn Direction
    | EndGame



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndGame ->
            ( { model | gameState = GameOver }, Cmd.none )

        EatApple ->
            ( { model | score = model.score + 20 }
            , Random.generate NewApple (appleGenerator model.arenaDimensions)
            )

        TimePenalty ->
            ( { model | score = model.score - 1 }, Cmd.none )

        Move ->
            let
                moveResult =
                    model.snake |> move model.arenaDimensions model.apple
            in
            case moveResult of
                Collision ->
                    model |> update EndGame

                Moved nextSnake ->
                    ( { model | snake = nextSnake }, Cmd.none )

                MovedAndFed nextSnake ->
                    { model | snake = nextSnake } |> update EatApple

        NewApple apple ->
            ( { model | apple = apple }, Cmd.none )

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


appleGenerator : ArenaDimensions -> Random.Generator Apple
appleGenerator { cols, rows } =
    Random.map2 Position (Random.int 1 cols) (Random.int 1 rows)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Running ->
            let
                moveSub =
                    Time.every (1000 / toFloat (speedPerSecond model.score)) (\_ -> Move)

                timePenaltySub =
                    Time.every 1000 (\_ -> TimePenalty)

                keyPressedSub =
                    Browser.Events.onKeyDown keyPressedDecoder
            in
            Sub.batch [ moveSub, timePenaltySub, keyPressedSub ]

        GameOver ->
            Sub.none


keyPressedDecoder : Decode.Decoder Msg
keyPressedDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyPressed


speedPerSecond : Int -> Int
speedPerSecond score =
    10 + (score // 200)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ titleView
        , scoreView model.score
        , gameArenaView model
        ]


titleView =
    let
        styles =
            [ style "text-align" "center", style "font-size" "40px", style "margin-bottom" "10px" ]
    in
    h1 styles [ Html.text "elm-snake" ]


scoreView : Int -> Html msg
scoreView score =
    let
        styles =
            [ style "text-align" "center", style "font-size" "24px", style "color" "red" ]
    in
    div styles [ Html.text ("score: " ++ String.fromInt score) ]


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
            , style "top" "55%"
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
    shapes [ fill Color.red ] [ renderTile arenaDimensions apple ]


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
        Running ->
            Color.green

        GameOver ->
            Color.red
