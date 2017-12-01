port module Input.Element exposing (..)

-- import Process
-- import Task exposing (Task)

import Vector as V exposing (Vec2, Position, Size, Box, v2)
import VirtualDom exposing (on, onWithOptions)

import Json.Decode exposing (..)



type alias ClientRect =
  { left   : Float
  , right  : Float
  , top    : Float
  , bottom : Float
  }

-- Events
toGeometry: ClientRect -> Box
toGeometry rect = Box (v2 rect.left rect.top) (v2 (rect.right - rect.left) (rect.bottom - rect.top))


decodeClientRect : Decoder ClientRect
decodeClientRect = map4 ClientRect (field "top" float) (field "left" float) (field "right" float) (field "top" float)

-- Ports
type alias Id = String

port askGeometry  : Id -> Cmd msg

port clientRect : ((Id, ClientRect) -> msg) -> Sub msg

geometry : String -> (Maybe Box -> msg) -> Sub msg
geometry id1 f = clientRect (\(id2, rect) -> if id1 == id2
  then f (Just (toGeometry rect))
  else f Nothing)
