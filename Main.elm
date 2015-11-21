import Html exposing (..)
import Html.Attributes as Attr exposing (..)


type alias State = { running : Bool }

view : State -> Html
view state =
  div [containerStyle] []

containerStyle = style [ ("width", "800px")
                       , ("height", "600px")]

main =
  Signal.map view state.signal

state : Signal.Mailbox State
state = Signal.mailbox { running = False}
