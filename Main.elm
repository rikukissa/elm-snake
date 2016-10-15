module Main (main) where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Keyboard
import Window
import Task exposing (Task)
import LocalStorage
import Time exposing (second)
import Array exposing (Array, fromList, toList)
import Random exposing (Seed)
import Signal
import String exposing (toInt)
import Result
import Set
import List exposing (..)
import Char

type alias Position =
  { x : Int
  , y : Int
  }

type alias Tick =
  Int

type alias Emoji =
  String

type Direction
  = Left
  | Right
  | Up
  | Down
  | None

type alias Input =
  { direction : Direction
  , tick : Tick
  , seed: Seed
  }

type alias Apple =
  { position: Maybe Position
  , bonus: Bool
  }

type alias Snake =
  { position : Position
  , previousPositions : List Position
  , points : Int
  , direction : Direction
  , lastPointAt : Tick
  }

type alias State =
  { running : Bool
  , logoVisible : Bool
  , snake : Snake
  , apple : Apple
  , overlays : Array (Tick, Emoji)
  , gameEndedAt : Tick
  }

framesPerSecond : Int
framesPerSecond =
  15

mapSize : Int
mapSize =
  20

initialState : State
initialState =
  { running = False
  , logoVisible = True
  , gameEndedAt = 0
  , overlays = fromList []
  , apple =
    { position = Nothing
    , bonus = False
    }
  , snake =
    { position =
      { x = round (toFloat mapSize / 3)
      , y = round (toFloat mapSize / 3)
      }
    , previousPositions = []
    , points = 0
    , lastPointAt = 0
    , direction = None
    }
  }

main : Signal Html
main =
  Signal.map3 view gameState Window.dimensions highscore

gameState : Signal.Signal State
gameState =
  Signal.foldp stepGame initialState input

{- High Score handling -}
currentPoints : Signal Int
currentPoints =
  Signal.map (\state -> state.snake.points) gameState
  |> Signal.merge highscoreLoaded.signal

highscore : Signal Int
highscore =
  Signal.foldp Basics.max 0 currentPoints

highscoreLoaded : Signal.Mailbox Int
highscoreLoaded = Signal.mailbox 0

port saveHighscore : Signal (Task LocalStorage.Error String)
port saveHighscore =
  highscore
  |> Signal.map toString
  |> Signal.map (LocalStorage.set "highscore")

port getHighscore : Task LocalStorage.Error ()
port getHighscore =
  let handle str =
    case str of
      Just s -> Result.withDefault 0 (String.toInt s)
        |> Signal.send highscoreLoaded.address
      Nothing -> Signal.send highscoreLoaded.address 0
  in
    (LocalStorage.get "highscore") `Task.andThen` handle


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

view : State -> (Int, Int) -> Int -> Html
view ({running, logoVisible, snake, apple, overlays} as state) (width, height) highscore =
  let
    canvasSize = Basics.min width height
    blockSize = round ((toFloat canvasSize) / (toFloat mapSize))
    blockStyle = toBlockStyle blockSize
    scale position =
      { x = position.x * blockSize
      , y = position.y * blockSize
      }

    snakeHead = div [class "head", blockStyle (scale snake.position)] [text "😎"]
    snakeBody = map (\position -> div [blockStyle (scale position)] [text "😂"]) snake.previousPositions
    snakeNode = snakeHead :: snakeBody

    appleNode = case apple.position of
      Just position -> div [blockStyle (scale position)] [text (if apple.bonus then "💎" else "🍔")]
      _ -> div [] []

    pointsNode = div []
      [ div [class "points"] [text (toString snake.points)]
      , div [class "highscore"] [text (toString highscore)]
      ]

    overlayNode = toList overlays
      |> List.map (\(_, emoji) -> div [class "overlay"] [text emoji])
      |> div []

    logoNode = if not logoVisible then
      img [src "logo.png", class "logo logo--hidden"] [] else
      img [src "logo.png", class "logo"] []
  in
    div [class "container", containerStyle canvasSize blockSize]
      [ div [class "entities"]
         (appleNode :: snakeNode)
      , pointsNode
      , overlayNode
      , logoNode
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



newApple : Apple -> Snake -> Seed -> Apple
newApple apple snake seed =
  let
    (bonusProbability, seed') = Random.generate (Random.float 0 1) seed
    (xPosition, seed'') = Random.generate (Random.int 0 (mapSize - 1)) seed'
    (yPosition, seed''') = Random.generate (Random.int 0 (mapSize - 1)) seed''
    newPosition = { x = xPosition
                  , y = yPosition
                  }
  in
    if touchesSnake snake newPosition then
      -- Get new position
      newApple apple snake seed'''
    else
      { bonus = bonusProbability > 0.9
      , position = Just newPosition
      }

touchesSnake : Snake -> Position -> Bool
touchesSnake snake position =
  any (\p -> p == position) (snake.position :: snake.previousPositions)

snakeTouchesApple : Snake -> Apple -> Bool
snakeTouchesApple snake apple =
  case apple.position of
    Just position -> touchesSnake snake position
    Nothing -> False

snakeTouchesItself : Snake -> Bool
snakeTouchesItself snake =
  List.any (\position -> position == snake.position) snake.previousPositions

stepApple : Apple -> Snake -> Input -> Apple
stepApple apple snake input =
  if snakeTouchesApple snake apple then
    newApple apple snake input.seed
  else
    apple

stepGame : Input -> State -> State
stepGame ({direction, tick, seed} as input) ({running, logoVisible, snake, apple, overlays, gameEndedAt} as state) =
  let
    running = state.running || direction /= None
    logoVisible = not running
    justStarted = not state.running && running

    updatedApple = if justStarted then newApple apple snake seed else stepApple apple snake input
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
          logoVisible = logoVisible,
          apple = updatedApple,
          snake = updatedSnake,
          overlays = updatedOverlays,
          gameEndedAt = updatedGameEndedAt }


tick : Signal.Signal Tick
tick =
  Time.fps framesPerSecond
  |> Signal.map (always 1)
  |> Signal.foldp (+) 0

toDirection : Char.KeyCode -> Direction
toDirection keyCode =
  case keyCode of
    37 -> Left
    38 -> Up
    39 -> Right
    40 -> Down
    _ -> None

currentDirection : (Maybe Char.KeyCode) -> Direction
currentDirection key =
  case key of
    Just k -> toDirection k
    _ -> None

keysDown' : Signal (Maybe Char.KeyCode)
keysDown' =
  Keyboard.keysDown
  |> Signal.map Set.toList
  |> Signal.map head

timeSeed : Signal Seed
timeSeed =
  Time.every second
  |> Signal.map round
  |> Signal.map Random.initialSeed

type Action
  = Remove
  | Add Direction

last' : List Direction -> Direction
last' buffer =
  head (reverse buffer)
  |> Maybe.withDefault None

handleBuffer : Action -> (Direction, List Direction) -> (Direction, List Direction)
handleBuffer action (lastRemoved, buffer) =
  case action of
    Remove ->
      ( Maybe.withDefault None (head buffer)
      , Maybe.withDefault [] (tail buffer)
      )

    Add k -> (lastRemoved, take 5 (append buffer [k]))

arrowKeys : Signal Direction
arrowKeys =
  let
    directions = Signal.map currentDirection keysDown'
      |> Signal.filter (\direction -> direction /= None) None

    merged = Signal.merge
      (Signal.map (always Remove) tick)
      (Signal.map Add directions)

    buffer = Signal.foldp handleBuffer (None, []) merged
      |> Signal.map fst
  in
    buffer


input : Signal.Signal Input
input =
  Signal.sampleOn tick (Signal.map3 Input arrowKeys tick timeSeed)
