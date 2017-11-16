module Input exposing (..)

-- import Html exposing (..)
-- import Html.Events exposing (..)
-- import Html.Attributes exposing (..)

import Vector exposing (..)

import Char
import Keyboard

import Input.Window as Window
import Input.Mouse as Mouse
import Set exposing (Set)

-- import Debug

type Event
  = MouseDown Mouse.Button
  | MouseUp Mouse.Button
  | MouseWheel Mouse.ScrollDelta

  | MouseMove Position

  | KeyDown Char.KeyCode
  | KeyUp Char.KeyCode
  | KeyPress Char.KeyCode

  | Focus Bool

  | Cancel


transform : (Position -> Position) -> Event -> Event
transform f event = case event of
  MouseMove position -> MouseMove (f position)
  _                  -> event


mapBindings : (State, Event) -> Event
mapBindings (state, event) = case event of
  KeyDown 27 -> Cancel
  _          -> event



subscriptions : (Event -> msg) -> Sub msg
subscriptions f = Sub.batch
  [ Keyboard.ups (KeyUp >> f)
  , Keyboard.downs (KeyDown >> f)
  , Window.onMouseUp (MouseUp >> f)
  , Window.onMouseDown (MouseDown >> f)
  , Window.onMouseWheel (MouseWheel >> f)
  , Window.onMouseMove (MouseMove >> f)
  , Window.onFocus (Focus >> f)
  ]


type alias State =
  { position : Vector
  , keys : Set Char.KeyCode
  }

init : State
init =
  { position  = Vector 0 0
  , keys = Set.empty
  }


update : Event -> State -> State
update input state = case input of
   MouseMove p -> { state | position = p }
   KeyDown k   -> { state | keys = Set.insert k state.keys }
   KeyUp k     -> { state | keys = Set.remove k state.keys }
   Focus _     -> { state | keys = Set.empty }
   _           -> state
