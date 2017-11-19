module Util exposing (..)

import Debug
import Json.Decode exposing (..)


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
