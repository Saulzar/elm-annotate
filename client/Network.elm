port module Network exposing (..)


import Json.Decode as Dec
import Json.Encode as Enc

import Types exposing (..)
import Json.Decode as Json exposing (Value)

--
-- listenResponse :  String -> (ServerMsg -> msg) -> Sub msg
-- listenResponse host f = WS.listen host <| \str ->
--     case  Dec.decodeString jsonDecServerMsg str of
--       Ok response -> f response
--       Err err -> Debug.log ("Decode response: " ++ err) (f <| Error err)


type Msg = Error String | Open | Close | Message ServerMsg | Retry


websocketHost : String -> String
websocketHost host = "ws://" ++ host ++ ":3000/ws"


port onOpen  : (Value -> msg) -> Sub msg
port onClose  : (Value -> msg) -> Sub msg
-- port onError : (String -> msg) -> Sub msg

port onMessage : (String -> msg) -> Sub msg


sendMsg : ClientMsg -> Cmd msg
sendMsg msg =  send_ (Enc.encode 0 (jsonEncClientMsg msg))


port send_ : String -> Cmd msg
port connect  : String -> Cmd msg


decodeMsg : String -> Msg
decodeMsg str =  case Json.decodeString jsonDecServerMsg str of
  Err e   -> Error ("decode error: " ++ e)
  Ok msg  -> Message msg


subscriptions : (Msg -> msg) -> Sub msg
subscriptions f = Sub.map f <| Sub.batch
  [ onOpen (always Open)
  , onClose (always Close)
  --, onError Error
  , onMessage decodeMsg
  ]
