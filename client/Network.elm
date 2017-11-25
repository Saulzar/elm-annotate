module Network exposing (..)


import Json.Decode as Dec
import Json.Encode as Enc

import WebSocket as WS
import Debug

import Types exposing (..)

import Process
import Task
import Time exposing (Time)

import Navigation as Nav exposing (Location)



request : String -> Request -> Cmd msg
request host req =  WS.send host (Enc.encode 0 (jsonEncRequest req))


listenResponse :  String -> (Response -> msg) -> Sub msg
listenResponse host f = WS.listen host <| \str ->
    case  Dec.decodeString jsonDecResponse str of
      Ok response -> f response
      Err err -> Debug.log ("Decode response: " ++ err) (f <| RespError err)


delay :  msg -> Time.Time -> Cmd msg
delay msg time = Process.sleep time |> Task.perform (always msg)


type alias State =
  { error     : Maybe String
  , recievedPong  : Int
  , pingCount : Int

  , lagging : Int
  , host : String
  }

timeoutTime : Time
timeoutTime = 2 * Time.second

type Msg = Timeout | Response Response

ping : String -> Int -> Cmd Msg
ping host n = Cmd.batch [request host (ReqPing n), delay Timeout timeoutTime]

init : Location -> (State, Cmd Msg)
init loc =
  let host = "ws://" ++ loc.hostname ++ ":3000"
  in ({ error = Nothing, recievedPong = -1, host = host, pingCount = 0, lagging = 0},  ping host 0)



update :  Msg -> State -> (State, Maybe Response, Cmd Msg)
update msg state = case msg of
  Timeout ->
    let count = state.pingCount + 1
    in ({state | pingCount = count, lagging = state.recievedPong - state.pingCount }, Nothing, ping state.host count)

  Response r -> case r of
    RespPong n -> ( {state | recievedPong = n} , Nothing, Cmd.none)
    _ -> (state, Just r, Cmd.none) 


subscriptions : State -> (Msg -> msg) -> Sub msg
subscriptions state f = Sub.map f <| Sub.batch
  [ listenResponse state.host Response ]
