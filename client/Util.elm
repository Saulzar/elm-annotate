module Util exposing (..)

import Debug
import Json.Decode exposing (..)

import List.Extra as List

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
