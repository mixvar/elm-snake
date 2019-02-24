module Main exposing (main)

import Browser
import Browser.Events
import Canvas
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import List
import Random
import Time



-- SETUP


increment val =
    val + 1


decrement val =
    val - 1


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
    , fruit : Fruit
    }


type GameState
    = Running
    | GameOver


type alias ArenaDimensions =
    { unitSize : Int, cols : Int, rows : Int }



-- MODEL -> DIRECTION


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



-- Model -> POSITION


type alias Position =
    { col : Int, row : Int }


type alias Fruit =
    Position


toPoint : ArenaDimensions -> Position -> Canvas.Point
toPoint { unitSize } { col, row } =
    let
        transform =
            decrement >> (*) unitSize >> toFloat
    in
    ( transform col, transform row )


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



-- MODEL -> SNAKE


type alias Snake =
    { body : List Position
    , direction : Direction
    , turnQueue : List Direction
    }


enqueueTurn : Direction -> Snake -> Snake
enqueueTurn direction snake =
    { snake | turnQueue = List.append snake.turnQueue [ direction ] }


type MoveResult
    = Moved Snake
    | MovedAndFed Snake
    | Collision


moveSnake : ArenaDimensions -> Fruit -> Snake -> MoveResult
moveSnake dimensions fruit snake =
    let
        turnedSnake =
            turnSnake snake

        maybeNextHead =
            snake.body
                |> List.head
                |> Maybe.map (transformPosition turnedSnake.direction)
                |> Maybe.map (toArenaPosition dimensions)

        snakeFed =
            maybeNextHead
                |> Maybe.map ((==) fruit)
                |> Maybe.withDefault False

        adjustBody =
            if snakeFed then
                identity

            else
                List.take (List.length snake.body)

        maybeNextSnake =
            maybeNextHead
                |> Maybe.map (\head -> head :: snake.body)
                |> Maybe.map adjustBody
                |> Maybe.map (\body -> { turnedSnake | body = body })
                |> Maybe.andThen validateCollisions
    in
    case ( maybeNextSnake, snakeFed ) of
        ( Just nextSnake, True ) ->
            MovedAndFed nextSnake

        ( Just nextSnake, False ) ->
            Moved nextSnake

        ( Nothing, _ ) ->
            Collision


turnSnake : Snake -> Snake
turnSnake snake =
    case List.head snake.turnQueue of
        Just nextDir ->
            let
                isTurnValid =
                    nextDir /= opposite snake.direction && nextDir /= snake.direction

                queue =
                    List.drop 1 snake.turnQueue
            in
            case isTurnValid of
                True ->
                    { snake | direction = nextDir, turnQueue = queue }

                False ->
                    turnSnake { snake | turnQueue = queue }

        Nothing ->
            snake


validateCollisions : Snake -> Maybe Snake
validateCollisions snake =
    case snake.body of
        head :: tail ->
            if List.member head tail then
                Nothing

            else
                Just snake

        _ ->
            Nothing



-- INIT


init : Void -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { gameState = Running
    , score = 0
    , arenaDimensions = ArenaDimensions 18 30 18
    , snake = initialSnake
    , fruit = Position 15 3
    }


initialSnake : Snake
initialSnake =
    { direction = Up
    , turnQueue = []
    , body = List.range 13 18 |> List.map (\y -> Position 15 y)
    }



-- MESSAGES


type Msg
    = Move
    | NewFruit Fruit
    | FruitEaten
    | TimePenalty
    | KeyPressed String
    | Turn Direction
    | EndGame
    | Restart



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndGame ->
            ( { model | gameState = GameOver }, Cmd.none )

        Restart ->
            init ()

        FruitEaten ->
            ( { model | score = model.score + 10 }
            , generateFruit model.arenaDimensions
            )

        TimePenalty ->
            ( { model | score = model.score - 1 }, Cmd.none )

        Move ->
            let
                moveResult =
                    model.snake |> moveSnake model.arenaDimensions model.fruit
            in
            case moveResult of
                Collision ->
                    model |> update EndGame

                Moved nextSnake ->
                    ( { model | snake = nextSnake }, Cmd.none )

                MovedAndFed nextSnake ->
                    { model | snake = nextSnake } |> update FruitEaten

        NewFruit fruit ->
            if List.member fruit model.snake.body then
                ( model, generateFruit model.arenaDimensions )

            else
                ( { model | fruit = fruit }, Cmd.none )

        KeyPressed key ->
            case model.gameState of
                Running ->
                    case directionFromKey key of
                        Just dir ->
                            model |> update (Turn dir)

                        _ ->
                            ( model, Cmd.none )

                GameOver ->
                    case key of
                        " " ->
                            model |> update Restart

                        _ ->
                            ( model, Cmd.none )

        Turn direction ->
            ( { model | snake = model.snake |> enqueueTurn direction }, Cmd.none )


generateFruit : ArenaDimensions -> Cmd Msg
generateFruit dims =
    Random.generate NewFruit (fruitGenerator dims)


fruitGenerator : ArenaDimensions -> Random.Generator Fruit
fruitGenerator { cols, rows } =
    Random.map2 Position (Random.int 1 cols) (Random.int 1 rows)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        moveSub =
            Time.every (1000 / toFloat (speedPerSecond model.score)) (\_ -> Move)

        timePenaltySub =
            Time.every 1000 (\_ -> TimePenalty)

        keyPressedSub =
            Browser.Events.onKeyDown keyPressedDecoder
    in
    case model.gameState of
        Running ->
            Sub.batch [ moveSub, timePenaltySub, keyPressedSub ]

        GameOver ->
            keyPressedSub


keyPressedDecoder : Decode.Decoder Msg
keyPressedDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyPressed


speedPerSecond : Int -> Int
speedPerSecond score =
    10 + (score // 250)



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ headerView model.score
        , gameoverView model.gameState
        , gameArenaView model
        ]


headerView : Int -> Html msg
headerView score =
    let
        containerStyles =
            [ style "text-align" "center", style "position" "relative", style "z-index" "10" ]

        titleStyles =
            [ style "font-size" "40px", style "margin" "5px", style "color" "#60B5CC" ]

        scoreStyles =
            [ style "font-size" "24px" ]
    in
    Html.div containerStyles
        [ Html.h1 titleStyles [ Html.text "elm-snake" ]
        , Html.div scoreStyles [ Html.text ("score: " ++ String.fromInt score) ]
        ]


gameoverView : GameState -> Html msg
gameoverView state =
    case state of
        GameOver ->
            let
                styles =
                    [ style "z-index" "10"
                    , style "position" "absolute"
                    , style "width" "100%"
                    , style "color" "red"
                    , style "top" "50%"
                    , style "transform" "translateY(-50%)"
                    , style "text-align" "center"
                    , style "font-size" "150%"
                    ]
            in
            Html.div styles
                [ Html.h1 [] [ Html.text "GAME OVER!" ]
                , Html.h3 [] [ Html.text "Hit space to go again" ]
                ]

        _ ->
            Html.text ""


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
        , renderFruit model
        , renderSnake model
        ]


renderBackground : Int -> Int -> Color -> Canvas.Renderable
renderBackground width height color =
    Canvas.shapes [ Canvas.fill color ] [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderFruit : Model -> Canvas.Renderable
renderFruit { fruit, arenaDimensions } =
    Canvas.shapes [ Canvas.fill (Color.rgb 0.99 0.91 0.13) ] [ renderTile arenaDimensions fruit ]


renderSnake : Model -> Canvas.Renderable
renderSnake model =
    Canvas.shapes [ Canvas.fill (snakeColor model.gameState) ]
        (model.snake.body |> List.map (renderTile model.arenaDimensions))


renderTile : ArenaDimensions -> Position -> Canvas.Shape
renderTile dims coords =
    renderSquare dims.unitSize (toPoint dims coords)


renderSquare : Int -> Canvas.Point -> Canvas.Shape
renderSquare size point =
    Canvas.rect point (toFloat size) (toFloat size)


snakeColor : GameState -> Color
snakeColor state =
    case state of
        Running ->
            Color.rgb 0.38 0.7 0.8

        GameOver ->
            Color.rgb 0.1 0.1 0.1
