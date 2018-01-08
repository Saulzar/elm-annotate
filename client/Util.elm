module Util exposing (..)

import Debug
import Json.Decode exposing (..)

import List.Extra as List

import Process
import Task
import Time

assertResult : Result String a -> a
assertResult r = case r of
     Ok x        -> x
     Err reason  -> Debug.crash (toString reason)

tryDecode : Decoder a -> Value -> a
tryDecode decoder value = assertResult (decodeValue decoder value)


noCmd : model -> (model, Cmd msg)
noCmd model = (model, Cmd.none)


applyMaybe : (a -> b -> b) -> Maybe a -> b -> b
applyMaybe f ma = case ma of
  Nothing -> identity
  Just a  -> f a


delay :  msg -> Time.Time -> Cmd msg
delay msg time = Process.sleep time |> Task.perform (always msg)


insertAt : Int -> a -> List a -> List a
insertAt i a xs = let (prev, after) = List.splitAt i xs in prev ++ (a :: after)

modifyAt : Int -> (a -> a) -> List a -> List a
modifyAt i f xs = let (prev, after) = List.splitAt i xs in case after of
  [] -> xs
  (a :: rest) -> prev ++ (f a :: rest)

removeAt : Int -> List a -> List a
removeAt i xs = let (prev, after) = List.splitAt i xs in case after of
  [] -> xs
  (a :: rest) -> prev ++ rest

maybeToList : Maybe a -> List a
maybeToList ma = case ma of
  Nothing -> []
  Just a  -> [a]


addCmd : Cmd msg -> (a, Cmd msg) -> (a, Cmd msg)
addCmd cmd (a, cmds) = (a, Cmd.batch [cmd, cmds])


withCmd : (a -> m -> (m, Cmd msg)) -> (a -> (m, Cmd msg) -> (m, Cmd msg))
withCmd f a (model1, cmd1) = let (model2, cmd2) = f a model1 in (model2, Cmd.batch [cmd1, cmd2])

foldUpdate : (a -> m -> (m, Cmd msg)) -> m -> List a -> (m, Cmd msg)
foldUpdate f model = List.foldr (withCmd f) (model, Cmd.none)
