module Input.Mouse exposing (..)

import Html exposing (..)
import Html.Events exposing (..)

import Json.Decode as Json exposing (Decoder, field, float, int, map2, map3)
-- import Process
-- import Task exposing (Task)

import Vector as V exposing (Position, Size, vec)


type alias Modifiers =
  { ctrl : Bool
  , alt  : Bool
  , meta : Bool
  , shift : Bool
  }

type alias State =
  { buttons   : Buttons
  , modifiers : Modifiers
  , position  : Position
  }


type Button
  = Left
  | Middle
  | Right
  | Button4
  | Button5

{-| Scroll offsets for the scroll events.
-}
type alias ScrollDelta =
  { dx : Float
  , dy : Float
  , dz : Float
  }

type alias Buttons =
  { left   : Bool
  , middle : Bool
  , right  : Bool
  , button4 : Bool
  , button5 : Bool
  }

setButton : Button -> Bool -> Buttons -> Buttons
setButton b isDown bs = case b of
  Left    -> { bs | left = isDown }
  Right   -> { bs | right = isDown }
  Middle  -> { bs | middle = isDown }
  Button4  -> { bs | button4 = isDown }
  Button5  -> { bs | button5 = isDown }



buttons : Buttons
buttons = { left = False, right = False, middle = False, button4 = False, button5 = False }


decodeClient : Decoder Position
decodeClient = map2 vec (field "clientX" float) (field "clientY" float)

decodePage : Decoder Position
decodePage = map2 vec (field "pageX" float) (field "pageY" float)


type alias Positions =
  { client : Position
  , page   : Position
  }

decodePositions : Decoder Positions
decodePositions = map2 Positions decodeClient decodePage


decodeScroll : Decoder ScrollDelta
decodeScroll = map3 ScrollDelta (field "deltaX" float) (field "deltaY" float) (field "deltaZ" float)


zoomBy : ScrollDelta -> Float
zoomBy deltas = (1 - deltas.dy / 500)

toButton : Int -> Button
toButton b = case b of
    0 -> Left
    1 -> Middle
    2 -> Right
    3 -> Button4
    _ -> Button5


decodeButton : Decoder Button
decodeButton =  Json.map toButton <| field "button" int


onMove : (Position -> msg) -> Attribute msg
onMove f = on "mousemove" (Json.map f decodeClient)

onDown : (Button -> msg) -> Attribute msg
onDown f = on "mousedown"  (Json.map f decodeButton)

onUp : (Button -> msg) -> Attribute msg
onUp f = on "mouseup"    (Json.map f decodeButton)

onWheel : (ScrollDelta -> msg)  -> Attribute msg
onWheel f = on "wheel"    (Json.map f decodeScroll)



on_ : String -> Decoder (Maybe msg) -> Attribute msg
on_ event decoder = onWithOptions event {stopPropagation = True, preventDefault = True}
  (decoder |> Json.andThen (\m -> case m of
    Nothing  -> Json.fail "ignored"
    Just msg -> Json.succeed msg))


onMove_ : (Position -> Maybe msg) -> Attribute msg
onMove_ f = on_ "mousemove" (Json.map f decodeClient)

onDown_ : (Button -> Maybe msg) -> Attribute msg
onDown_ f = on_ "mousedown"  (Json.map f decodeButton)

onUp_ : (Button -> Maybe msg) -> Attribute msg
onUp_ f = on_ "mouseup"    (Json.map f decodeButton)

onWheel_ : (ScrollDelta -> Maybe msg)  -> Attribute msg
onWheel_ f = on_ "wheel"    (Json.map f decodeScroll)
