port module Input.Size exposing (..)

-- import Process
-- import Task exposing (Task)

import Vector as V exposing (Vector, Position, Size, Box)

import Json.Decode exposing (..)



type alias ClientRect =
  { left   : Float
  , right  : Float
  , top    : Float
  , bottom : Float
  }
 
-- Events
toGeometry: ClientRect -> Box
toGeometry rect = Box (Vector rect.left rect.top) (Vector (rect.right - rect.left) (rect.bottom - rect.top))


decodeClientRect : Decoder ClientRect
decodeClientRect = map4 ClientRect (field "top" float) (field "left" float) (field "right" float) (field "top" float)

-- Ports

port askGeometry  : String -> Cmd msg

port clientRect : ((String, ClientRect) -> msg) -> Sub msg

geometry : String -> (Maybe Box -> msg) -> Sub msg
geometry id1 f = clientRect (\(id2, rect) -> if id1 == id2
  then f (Just (toGeometry rect))
  else f Nothing)
