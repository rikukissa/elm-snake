import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Debug exposing (..)
import Keyboard
import Time
import Random
import Signal
import Set
import List exposing (..)
import Char

type alias Position = { x : Int, y : Int }
type Direction = Left | Right | Up | Down | None

type alias Apple = { seed: Random.Seed, position: Maybe Position }

type alias Snake =
  { position : Position
  , previousPositions : List Position
  , points : Int
  , direction : Direction
  }

type alias State =
  { running : Bool
  , snake : Snake
  , apple : Apple
  }

speedFactor = 1
framesPerSecond = 10

canvasSize = 600
mapSize = 20
blockSize = round (canvasSize / mapSize)

initialState =
  { running = False
  , apple =
    { seed = Random.initialSeed 123
    , position = Nothing
    }
  , snake =
    { position =
      { x = round (mapSize / 2)
      , y = round (mapSize / 2)
      }
    , previousPositions = []
    , points = 0
    , direction = None
    }
  }

view : State -> Html
view ({running, snake, apple} as state) =
  let
    wormHead = div [snakeStyle snake] []
    wormBody = map (\position -> div [tailStyle position] []) snake.previousPositions
    wormNode = wormHead :: wormBody

    appleNode = case apple.position of
      Just position -> div [appleStyle position] []
      _ -> div [] []

  in
    div [containerStyle]
      (appleNode :: wormNode)

toPixels : a -> String
toPixels value =
  toString value ++ "px"

snakeStyle : Snake -> Attribute
snakeStyle snake = style [ ("width", toPixels blockSize)
                         , ("height", toPixels blockSize)
                         , ("position", "absolute")
                         , ("top", toPixels (blockSize * snake.position.y))
                         , ("left", toPixels (blockSize * snake.position.x))
                         , ("background", "red")]

tailStyle : Position -> Attribute
tailStyle position = style [ ("width", toPixels blockSize)
                           , ("height", toPixels blockSize)
                           , ("position", "absolute")
                           , ("top", toPixels (blockSize * position.y))
                           , ("left", toPixels (blockSize * position.x))
                           , ("background", "blue")
                           ]

appleStyle : Position -> Attribute
appleStyle position = style [ ("width", toPixels blockSize)
                           , ("height", toPixels blockSize)
                           , ("position", "absolute")
                           , ("top", toPixels (blockSize * position.y))
                           , ("left", toPixels (blockSize * position.x))
                           , ("background", "blue")
                           ]

containerStyle : Attribute
containerStyle = style [ ("width", toPixels canvasSize)
                       , ("height", toPixels canvasSize)
                       , ("position", "relative")
                       , ("background", "#ccc")]

main =
  Signal.map view gameState

gameState : Signal.Signal State
gameState =
  Signal.foldp stepGame initialState input
  |> Signal.map (log "foo")

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

stepSnake : Input -> Snake -> Apple -> Snake
stepSnake ({direction} as input) ({position, previousPositions, points} as snake) apple =
  let
    newDirection = if direction == None then snake.direction else direction
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
    newPoints = if snakeTouchesApple snake apple then points + 1 else points
  in
    { position = cappedPosition
    , previousPositions = slicedPreviousPositions
    , points = newPoints
    , direction = newDirection
    }



newApple : Apple -> Apple
newApple apple =
  let
    (xPosition, seed') = Random.generate (Random.int 0 (mapSize - 1)) apple.seed
    (yPosition, seed'') = Random.generate (Random.int 0 (mapSize - 1)) seed'
  in
    { seed = seed''
    , position = Just { x = xPosition
                      , y = yPosition
                      }
    }

snakeTouchesApple : Snake -> Apple -> Bool
snakeTouchesApple snake apple =
  apple.position == Just snake.position

stepApple : Apple -> Snake -> Apple
stepApple apple snake =
  if snakeTouchesApple snake apple then
    newApple apple
  else
    apple

stepGame : Input -> State -> State
stepGame ({direction, delta} as input) ({running, snake, apple} as state) =
  let
    running = state.running || direction /= None
    justStarted = not state.running && running
    updatedApple = if justStarted then newApple apple else stepApple apple snake
    updatedSnake = stepSnake input snake apple
  in
    { state |
        running = running,
        apple = updatedApple,
        snake = updatedSnake }


delta : Signal.Signal Float
delta = Time.fps framesPerSecond
        |> Signal.map Time.inSeconds
        |> Signal.map (\d -> speedFactor * d)

type alias Input = { direction : Direction , delta : Time.Time }

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
  case keys of
    [xs] -> toDirection xs
    [] -> None
    _ -> None

arrowKeys = Signal.map (Set.toList >> currentDirection) Keyboard.keysDown

input : Signal.Signal Input
input = Signal.sampleOn delta (Signal.map2 Input arrowKeys delta)

