module Common exposing (..)

import Time.DateTime as DateTime
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- Common type synonyms used in both client/server


type alias DocName = String
type alias ObjId = (Int, Int)
type alias ClientId = Int

type alias DateTime = DateTime.DateTime


jsonDecObjId : Decoder (Int, Int)
jsonDecObjId = Decode.map2 (,) (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)

jsonEncObjId : (Int, Int) -> Value
jsonEncObjId (a, b) = Encode.list [Encode.int a, Encode.int b]


jsonDecDateTime : Decoder DateTime
jsonDecDateTime = Decode.string |> Decode.andThen
  (\str -> case DateTime.fromISO8601 str of
    Ok value -> Decode.succeed value
    Err err  -> Decode.fail err)


jsonEncDateTime : DateTime -> Value
jsonEncDateTime date = Encode.string (DateTime.toISO8601 date)
