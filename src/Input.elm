module Input exposing (..)

-- import Html exposing (..)
-- import Html.Events exposing (..)
-- import Html.Attributes exposing (..)

import Vector exposing (..)

import Keyboard.Key as Key exposing (Key)
import Keyboard.KeySet as KeySet exposing (KeySet)

import Input.Window as Window
import Input.Mouse as Mouse

import List
-- import Debug

type Event
  = MouseDown Mouse.Button
  | MouseUp Mouse.Button
  | Click Mouse.Button
  | MouseWheel Mouse.ScrollDelta

  | MouseMove Position

  | KeyDown Key
  | KeyUp Key
  | KeyPress Key

  | Focus Bool


type alias Binding = {key : Key, modifiers : List Key}



matchKey : (Key, State) -> Binding -> Bool
matchKey (pressed, state) bind = bind.key == pressed &&
    List.all (flip KeySet.member state.keys) bind.modifiers


matchKeys : (Event, State) -> List (Binding, a) -> Maybe a
matchKeys (e, state) bindings = case e of
  KeyDown k -> List.head
    <| List.map Tuple.second
    <| List.filter (Tuple.first >> matchKey (k, state)) bindings
  _         -> Nothing


transform : (Position -> Position) -> Event -> Event
transform f event = case event of
  MouseMove position -> MouseMove (f position)
  _                  -> event




subscriptions : (Event -> msg) -> Sub msg
subscriptions f = Sub.batch
  [ Key.onKeyUp (KeyUp >> f)
  , Key.onKeyDown (KeyDown >> f)
  , Window.onMouseUp (MouseUp >> f)
  , Window.onMouseDown (MouseDown >> f)
  , Window.onClick (Click >> f)
  , Window.onMouseWheel (MouseWheel >> f)
  , Window.onMouseMove (MouseMove >> f) 
  , Window.onFocus (Focus >> f)
  ]


type alias State =
  { position : Vector
  , keys : KeySet
  }

init : State
init =
  { position  = Vector 0 0
  , keys = KeySet.empty
  }


update : Event -> State -> State
update input state = case input of
   MouseMove p -> { state | position = p }
   KeyDown k   -> { state | keys = KeySet.insert k state.keys }
   KeyUp k     -> { state | keys = KeySet.remove k state.keys }
   Focus _     -> { state | keys = KeySet.empty }
   _           -> state
