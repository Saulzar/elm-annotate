port module Input.Window exposing (onMouseDown, onMouseUp, onMouseMove, onMouseWheel, onFocus, setCapture, releaseCapture)

-- import Process
-- import Task exposing (Task)

import Vector as V exposing (Vector, Position, Size)
import Input.Mouse exposing (..)

import Json.Decode as Json exposing (Value, Decoder, decodeValue)

import Util exposing (..)


-- Events

onMouseDown : (Button -> msg) -> Sub msg
onMouseDown f = windowMouseDown (tryDecode decodeButton >> f)

onMouseUp : (Button -> msg) -> Sub msg
onMouseUp f = windowMouseUp (tryDecode decodeButton >> f)

onFocus : (Bool -> msg) -> Sub msg
onFocus f = windowFocus (tryDecode Json.bool >> f)

onMouseWheel : (ScrollDelta -> msg) -> Sub msg
onMouseWheel f = windowMouseWheel (tryDecode decodeScroll >> f)


onMouseMove : (Position -> msg) -> Sub msg
onMouseMove f = windowMouseMove (tryDecode decodeClient >> f)



-- Ports


port windowMouseWheel  : (Value -> msg) -> Sub msg
port windowMouseDown  : (Value -> msg) -> Sub msg
port windowMouseUp  : (Value -> msg) -> Sub msg
port windowFocus  : (Value -> msg) -> Sub msg
port windowMouseMove : (Value -> msg) -> Sub msg


port setCapture  : String -> Cmd msg
port releaseCapture  : () -> Cmd msg
