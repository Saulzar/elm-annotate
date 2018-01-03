port module Network exposing (..)


import Json.Decode as Dec
import Json.Encode as Enc

import WebSocket as WS
import Debug

import Types exposing (..)

import Process
import Task
import Time exposing (Time)

import Navigation as Nav exposing (Location)

import Json.Decode as Json exposing (Value)

--
-- listenResponse :  String -> (ServerMsg -> msg) -> Sub msg
-- listenResponse host f = WS.listen host <| \str ->
--     case  Dec.decodeString jsonDecServerMsg str of
--       Ok response -> f response
--       Err err -> Debug.log ("Decode response: " ++ err) (f <| Error err)


delay :  msg -> Time.Time -> Cmd msg
delay msg time = Process.sleep time |> Task.perform (always msg)



type Msg = Error String | Connected | Disconnected | FromServer ServerMsg


hostname : Location -> String
hostname loc = "ws://" ++ loc.hostname ++ ":3000/ws"


port onConnect  : (Value -> msg) -> Sub msg
port onDisconnect  : (Value -> msg) -> Sub msg
port onError : (String -> msg) -> Sub msg

port onResponse : (String -> msg) -> Sub msg


request : ClientMsg -> Cmd msg
request msg =  send_ (Enc.encode 0 (jsonEncClientMsg msg))


port send_ : String -> Cmd msg
port connect  : String -> Cmd msg



subscriptions : (Msg -> msg) -> Sub msg
subscriptions f = Sub.map f <| Sub.batch
  [ onConnect (always Connected)
  , onDisconnect (always Disconnected)
  ]
