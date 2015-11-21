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

type alias Snake =
  { position : Position
  , previousPositions : List Position
  , points : Int
  , direction : Direction
  }

type alias State =
  { running : Bool
  , snake : Snake
  }

speedFactor = 1
framesPerSecond = 10

canvasSize = 600
mapSize = 20
blockSize = round (canvasSize / mapSize)

initialState =
  { running = False
  , snake =
    { position =
      { x = 0
      , y = 0
      }
    , previousPositions = []
    , points = 0
    , direction = None
    }
  }

view : State -> Html
view ({running, snake} as state) =
  let
    tail = map (\position -> div [tailStyle position] []) snake.previousPositions
  in
    div [containerStyle]
      (div [snakeStyle snake] [] :: tail)

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

stepSnake : Input -> Snake -> Snake
stepSnake ({direction} as input) ({position, previousPositions} as snake) =
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
    slicedPreviousPositions = drop (length previousPositions - snake.points) previousPositions
  in
    { position = cappedPosition
    , previousPositions = slicedPreviousPositions
    , points = snake.points
    , direction = newDirection
    }


stepGame : Input -> State -> State
stepGame ({direction, delta} as input) ({running, snake} as state) =
  let
    running = state.running || direction /= None
  in
    { state |
        running = running,
        snake = stepSnake input snake }


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
  |> Signal.map (log "foo")
