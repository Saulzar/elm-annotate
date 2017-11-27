module Types exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Vec2  =
   { x: Float
   , y: Float
   }

jsonDecVec2 : Json.Decode.Decoder ( Vec2 )
jsonDecVec2 =
   ("x" := Json.Decode.float) >>= \px ->
   ("y" := Json.Decode.float) >>= \py ->
   Json.Decode.succeed {x = px, y = py}

jsonEncVec2 : Vec2 -> Value
jsonEncVec2  val =
   Json.Encode.object
   [ ("x", Json.Encode.float val.x)
   , ("y", Json.Encode.float val.y)
   ]



type Edit  =
    Add Int Object
    | Delete Int

jsonDecEdit : Json.Decode.Decoder ( Edit )
jsonDecEdit =
    let jsonDecDictEdit = Dict.fromList
            [ ("Add", Json.Decode.map2 Add (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (jsonDecObject)))
            , ("Delete", Json.Decode.map Delete (Json.Decode.int))
            ]
    in  decodeSumObjectWithSingleField  "Edit" jsonDecDictEdit

jsonEncEdit : Edit -> Value
jsonEncEdit  val =
    let keyval v = case v of
                    Add v1 v2 -> ("Add", encodeValue (Json.Encode.list [Json.Encode.int v1, jsonEncObject v2]))
                    Delete v1 -> ("Delete", encodeValue (Json.Encode.int v1))
    in encodeSumObjectWithSingleField keyval val



type Object  =
    Point {position: Vec2, radius: Float}
    | Box {min: Vec2, max: Vec2}

jsonDecObject : Json.Decode.Decoder ( Object )
jsonDecObject =
    let jsonDecDictObject = Dict.fromList
            [ ("Point", Json.Decode.map Point (   ("position" := jsonDecVec2) >>= \pposition ->    ("radius" := Json.Decode.float) >>= \pradius ->    Json.Decode.succeed {position = pposition, radius = pradius}))
            , ("Box", Json.Decode.map Box (   ("min" := jsonDecVec2) >>= \pmin ->    ("max" := jsonDecVec2) >>= \pmax ->    Json.Decode.succeed {min = pmin, max = pmax}))
            ]
    in  decodeSumObjectWithSingleField  "Object" jsonDecDictObject

jsonEncObject : Object -> Value
jsonEncObject  val =
    let keyval v = case v of
                    Point vs -> ("Point", encodeObject [("position", jsonEncVec2 vs.position), ("radius", Json.Encode.float vs.radius)])
                    Box vs -> ("Box", encodeObject [("min", jsonEncVec2 vs.min), ("max", jsonEncVec2 vs.max)])
    in encodeSumObjectWithSingleField keyval val



type alias Document  =
   { undos: (List Edit)
   , redos: (List Edit)
   , instances: (Dict Int Object)
   }

jsonDecDocument : Json.Decode.Decoder ( Document )
jsonDecDocument =
   ("undos" := Json.Decode.list (jsonDecEdit)) >>= \pundos ->
   ("redos" := Json.Decode.list (jsonDecEdit)) >>= \predos ->
   ("instances" := decodeMap (Json.Decode.int) (jsonDecObject)) >>= \pinstances ->
   Json.Decode.succeed {undos = pundos, redos = predos, instances = pinstances}

jsonEncDocument : Document -> Value
jsonEncDocument  val =
   Json.Encode.object
   [ ("undos", (Json.Encode.list << List.map jsonEncEdit) val.undos)
   , ("redos", (Json.Encode.list << List.map jsonEncEdit) val.redos)
   , ("instances", (encodeMap (Json.Encode.int) (jsonEncObject)) val.instances)
   ]



type Request  =
    ReqDataset 
    | ReqOpen String
    | ReqEdit Edit
    | ReqPing Int

jsonDecRequest : Json.Decode.Decoder ( Request )
jsonDecRequest =
    let jsonDecDictRequest = Dict.fromList
            [ ("ReqDataset", Json.Decode.succeed ReqDataset)
            , ("ReqOpen", Json.Decode.map ReqOpen (Json.Decode.string))
            , ("ReqEdit", Json.Decode.map ReqEdit (jsonDecEdit))
            , ("ReqPing", Json.Decode.map ReqPing (Json.Decode.int))
            ]
    in  decodeSumObjectWithSingleField  "Request" jsonDecDictRequest

jsonEncRequest : Request -> Value
jsonEncRequest  val =
    let keyval v = case v of
                    ReqDataset  -> ("ReqDataset", encodeValue (Json.Encode.list []))
                    ReqOpen v1 -> ("ReqOpen", encodeValue (Json.Encode.string v1))
                    ReqEdit v1 -> ("ReqEdit", encodeValue (jsonEncEdit v1))
                    ReqPing v1 -> ("ReqPing", encodeValue (Json.Encode.int v1))
    in encodeSumObjectWithSingleField keyval val



type Response  =
    RespDataset Dataset
    | RespOpen String Document
    | RespError String
    | RespPong Int

jsonDecResponse : Json.Decode.Decoder ( Response )
jsonDecResponse =
    let jsonDecDictResponse = Dict.fromList
            [ ("RespDataset", Json.Decode.map RespDataset (jsonDecDataset))
            , ("RespOpen", Json.Decode.map2 RespOpen (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (jsonDecDocument)))
            , ("RespError", Json.Decode.map RespError (Json.Decode.string))
            , ("RespPong", Json.Decode.map RespPong (Json.Decode.int))
            ]
    in  decodeSumObjectWithSingleField  "Response" jsonDecDictResponse

jsonEncResponse : Response -> Value
jsonEncResponse  val =
    let keyval v = case v of
                    RespDataset v1 -> ("RespDataset", encodeValue (jsonEncDataset v1))
                    RespOpen v1 v2 -> ("RespOpen", encodeValue (Json.Encode.list [Json.Encode.string v1, jsonEncDocument v2]))
                    RespError v1 -> ("RespError", encodeValue (Json.Encode.string v1))
                    RespPong v1 -> ("RespPong", encodeValue (Json.Encode.int v1))
    in encodeSumObjectWithSingleField keyval val



type alias ImageInfo  =
   { file: String
   , annotated: Bool
   }

jsonDecImageInfo : Json.Decode.Decoder ( ImageInfo )
jsonDecImageInfo =
   ("file" := Json.Decode.string) >>= \pfile ->
   ("annotated" := Json.Decode.bool) >>= \pannotated ->
   Json.Decode.succeed {file = pfile, annotated = pannotated}

jsonEncImageInfo : ImageInfo -> Value
jsonEncImageInfo  val =
   Json.Encode.object
   [ ("file", Json.Encode.string val.file)
   , ("annotated", Json.Encode.bool val.annotated)
   ]



type alias Config  =
   { extensions: (List String)
   }

jsonDecConfig : Json.Decode.Decoder ( Config )
jsonDecConfig =
   ("extensions" := Json.Decode.list (Json.Decode.string)) >>= \pextensions ->
   Json.Decode.succeed {extensions = pextensions}

jsonEncConfig : Config -> Value
jsonEncConfig  val =
   Json.Encode.object
   [ ("extensions", (Json.Encode.list << List.map Json.Encode.string) val.extensions)
   ]



type alias Dataset  =
   { path: String
   , images: (List ImageInfo)
   , config: Config
   }

jsonDecDataset : Json.Decode.Decoder ( Dataset )
jsonDecDataset =
   ("path" := Json.Decode.string) >>= \ppath ->
   ("images" := Json.Decode.list (jsonDecImageInfo)) >>= \pimages ->
   ("config" := jsonDecConfig) >>= \pconfig ->
   Json.Decode.succeed {path = ppath, images = pimages, config = pconfig}

jsonEncDataset : Dataset -> Value
jsonEncDataset  val =
   Json.Encode.object
   [ ("path", Json.Encode.string val.path)
   , ("images", (Json.Encode.list << List.map jsonEncImageInfo) val.images)
   , ("config", jsonEncConfig val.config)
   ]



