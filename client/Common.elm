module Common exposing (..)

import Time.DateTime as DateTime
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


-- Common type synonyms used in both client/server
type alias Vec = { x : Float, y : Float }


type alias DocName = String
type alias ObjId = Int
type alias ClientId = Int

type alias DateTime = DateTime.DateTime


jsonEncClientId : ClientId -> Value
jsonEncClientId = Encode.int

jsonDecClientId : Decoder ClientId
jsonDecClientId = Decode.int


jsonEncDocName : DocName -> Value
jsonEncDocName = Encode.string

jsonDecDocName : Decoder DocName
jsonDecDocName = Decode.string

jsonDecObjId : Decoder ObjId
jsonDecObjId = Decode.int

jsonEncObjId : ObjId -> Value
jsonEncObjId = Encode.int


jsonDecVec : Decoder Vec
jsonDecVec = Decode.map2 Vec (Decode.index 0 Decode.float) (Decode.index 1 Decode.float)

jsonEncVec : Vec -> Value
jsonEncVec v = Encode.list [Encode.float v.x, Encode.float v.y]

jsonDecDateTime : Decoder DateTime
jsonDecDateTime = Decode.string |> Decode.andThen
  (\str -> case DateTime.fromISO8601 str of
    Ok value -> Decode.succeed value
    Err err  -> Decode.fail err)


jsonEncDateTime : DateTime -> Value
jsonEncDateTime date = Encode.string (DateTime.toISO8601 date)
