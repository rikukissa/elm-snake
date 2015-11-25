import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Keyboard
import Window
import Time
import Array exposing (Array, fromList, toList)
import Random
import Signal
import Set
import List exposing (..)
import Char

type alias Position = { x : Int, y : Int }
type alias Tick = Int
type alias Emoji = String
type Direction = Left | Right | Up | Down | None

type alias Apple = { seed: Random.Seed, position: Maybe Position, bonus: Bool }

type alias Snake =
  { position : Position
  , previousPositions : List Position
  , points : Int
  , direction : Direction
  , lastPointAt : Tick
  }

type alias State =
  { running : Bool
  , snake : Snake
  , apple : Apple
  , overlays : Array (Tick, Emoji)
  , gameEndedAt : Tick
  }

speedFactor = 1
framesPerSecond = 15
mapSize = 20

initialState =
  { running = False
  , gameEndedAt = 0
  , overlays = fromList []
  , apple =
    { seed = Random.initialSeed 123
    , position = Nothing
    , bonus = False
    }
  , snake =
    { position =
      { x = round (mapSize / 2)
      , y = round (mapSize / 2)
      }
    , previousPositions = []
    , points = 0
    , lastPointAt = 0
    , direction = None
    }
  }

getOverlay : Int -> Maybe String
getOverlay points =
  if points == 1 then
    Just "👌"
  else if points == 2 then
    Just "🙌"
  else if points == 5 then
    Just "👍"
  else if points == 10 then
    Just "😍"
  else if points == 20 then
    Just "💪"
  else if points == 50 then
    Just "👏"
  else if points == 75 then
    Just "🎉"
  else if points == 100 then
    Just "💎"
  else if points == 150 then
    Just "🌟"
  else
    Nothing

view : State -> (Int, Int) -> Html
view ({running, snake, apple, overlays} as state) (width, height) =
  let
    canvasSize = Basics.min width height
    blockSize = round ((toFloat canvasSize) / mapSize)
    blockStyle = toBlockStyle blockSize
    scale position =
      { x = position.x * blockSize
      , y = position.y * blockSize
      }

    snakeHead = div [class "head", blockStyle (scale snake.position)] [text "🐤"]
    snakeBody = map (\position -> div [blockStyle (scale position)] [text "😛"]) snake.previousPositions
    snakeNode = snakeHead :: snakeBody

    appleNode = case apple.position of
      Just position -> div [blockStyle (scale position)] [text (if apple.bonus then "💎" else "🍔")]
      _ -> div [] []

    pointsNode = div [class "points"] [text (toString snake.points)]

    overlayNode = toList overlays
      |> List.map (\(_, emoji) -> div [class "overlay"] [text emoji])
      |> div []

  in
    div [class "container", containerStyle canvasSize blockSize]
      [ node "link" [rel "stylesheet", href "style.css"] []
      , div []
         (appleNode :: snakeNode)
      , pointsNode
      , overlayNode
      ]

toPixels : a -> String
toPixels value =
  toString value ++ "px"

toBlockStyle : Int -> Position -> Attribute
toBlockStyle blockSize position =
  style [ ("width", toPixels blockSize)
        , ("height", toPixels blockSize)
        , ("top", toPixels position.y)
        , ("left", toPixels position.x)
        , ("position", "absolute")
        ]

containerStyle : Int -> Int -> Attribute
containerStyle canvasSize blockSize = style [ ("width", toPixels canvasSize)
                                            , ("height", toPixels canvasSize)
                                            , ("font-size", toPixels blockSize)
                                            ]

main =
  Signal.map2 view gameState Window.dimensions

gameState : Signal.Signal State
gameState =
  Signal.foldp stepGame initialState input

cap : Int -> Int
cap num =
  if num < 0 then
    mapSize + num
  else if num >= mapSize then
    num - mapSize
  else
    num

capPosition : Position -> Position
capPosition {x, y} =
  { x = cap x
  , y = cap y
  }


updateDirection : Direction -> Direction -> Direction
updateDirection old new =
  if new == None ||
    (old == Up && new == Down) ||
    (old == Down && new == Up) ||
    (old == Left && new == Right) ||
    (old == Right && new == Left) then
    old
  else new

stepSnake : Input -> Snake -> Apple -> Snake
stepSnake ({direction, tick} as input) ({position, previousPositions, points} as snake) apple =
  let
    newDirection = updateDirection snake.direction direction
    newPosition =
      { x = case snake.direction of
                  Left -> position.x - 1
                  Right -> position.x + 1
                  _ -> position.x
      , y = case snake.direction of
                  Up -> position.y - 1
                  Down -> position.y + 1
                  _ -> position.y
      }
    cappedPosition = capPosition newPosition
    previousPositions = snake.position :: snake.previousPositions
    slicedPreviousPositions = take snake.points previousPositions

    gotPoint = snakeTouchesApple snake apple
    applePoints = if apple.bonus then 10 else 1
    newPoints = if gotPoint then points + applePoints else points
  in
    { position = cappedPosition
    , previousPositions = slicedPreviousPositions
    , points = newPoints
    , direction = newDirection
    , lastPointAt = if gotPoint then tick else snake.lastPointAt
    }



newApple : Apple -> Apple
newApple apple =
  let
    (bonusProbability, seed) = Random.generate (Random.float 0 1) apple.seed
    (xPosition, seed') = Random.generate (Random.int 0 (mapSize - 1)) seed
    (yPosition, seed'') = Random.generate (Random.int 0 (mapSize - 1)) seed'
  in
    { seed = seed''
    , bonus = bonusProbability > 0.9
    , position = Just { x = xPosition
                      , y = yPosition
                      }
    }

snakeTouchesApple : Snake -> Apple -> Bool
snakeTouchesApple snake apple =
  apple.position == Just snake.position ||
    List.any (\position -> apple.position == Just position) snake.previousPositions

snakeTouchesItself : Snake -> Bool
snakeTouchesItself snake =
  List.any (\position -> position == snake.position) snake.previousPositions

stepApple : Apple -> Snake -> Apple
stepApple apple snake =
  if snakeTouchesApple snake apple then
    newApple apple
  else
    apple

stepGame : Input -> State -> State
stepGame ({direction, tick} as input) ({running, snake, apple, overlays, gameEndedAt} as state) =
  let
    running = state.running || direction /= None
    justStarted = not state.running && running

    updatedApple = if justStarted then newApple apple else stepApple apple snake
    updatedSnake = stepSnake input snake apple

    gameHasEnded = gameEndedAt /= 0
    gameEnded = gameHasEnded || snakeTouchesItself updatedSnake
    updatedGameEndedAt = if gameEnded && (not gameHasEnded) then tick else gameEndedAt

    newOverlays = if updatedSnake.lastPointAt > snake.lastPointAt then
      case getOverlay updatedSnake.points of
        Just emoji -> Array.push (tick, emoji) overlays
        Nothing -> overlays
    else if gameEnded then
      Array.push (tick, "👷") overlays
    else
      overlays

    updatedOverlays = Array.filter (\(createdAt, _) -> tick - createdAt < (framesPerSecond * 3)) newOverlays
  in
    if gameHasEnded && tick - updatedGameEndedAt > (framesPerSecond * 3) then
      initialState
    else if gameHasEnded then
      state
    else
      { state |
          running = if gameEnded then False else running,
          apple = updatedApple,
          snake = updatedSnake,
          overlays = updatedOverlays,
          gameEndedAt = updatedGameEndedAt }


tick : Signal.Signal Tick
tick = Time.fps framesPerSecond
        |> Signal.map (always 1)
        |> Signal.foldp (+) 0

type alias Input = { direction : Direction , tick : Tick }

toDirection : Char.KeyCode -> Direction
toDirection keyCode =
  case keyCode of
    37 -> Left
    38 -> Up
    39 -> Right
    40 -> Down
    _ -> None

currentDirection : (List Char.KeyCode) -> Direction
currentDirection keys =
  case reverse keys of
    [xs] -> toDirection xs
    hd::tl -> toDirection hd
    [] -> None

sortByRecentness : (List Char.KeyCode) -> (List Char.KeyCode) -> (List Char.KeyCode)
sortByRecentness newKeys oldKeys =
  List.filter (\code -> (not << (flip member) oldKeys) code) newKeys
  |> List.append (List.filter (\code -> member code newKeys) oldKeys)

keysDown' : Signal (List Char.KeyCode)
keysDown' = Keyboard.keysDown
  |> Signal.map Set.toList
  |> Signal.foldp sortByRecentness []

arrowKeys : Signal Direction
arrowKeys = Signal.map currentDirection keysDown'

input : Signal.Signal Input
input = Signal.sampleOn tick (Signal.map2 Input arrowKeys tick)

