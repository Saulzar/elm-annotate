module Types exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)
import Common exposing (..)

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



type alias Box  =
   { position: Vec2
   , size: Vec2
   }

jsonDecBox : Json.Decode.Decoder ( Box )
jsonDecBox =
   ("position" := jsonDecVec2) >>= \pposition ->
   ("size" := jsonDecVec2) >>= \psize ->
   Json.Decode.succeed {position = pposition, size = psize}

jsonEncBox : Box -> Value
jsonEncBox  val =
   Json.Encode.object
   [ ("position", jsonEncVec2 val.position)
   , ("size", jsonEncVec2 val.size)
   ]



type Edit  =
    Add ObjId Object
    | Delete ObjId
    | Transform (List ObjId) Float Vec2
    | Many (List Edit)

jsonDecEdit : Json.Decode.Decoder ( Edit )
jsonDecEdit =
    let jsonDecDictEdit = Dict.fromList
            [ ("Add", Json.Decode.lazy (\_ -> Json.Decode.map2 Add (Json.Decode.index 0 (jsonDecObjId)) (Json.Decode.index 1 (jsonDecObject))))
            , ("Delete", Json.Decode.lazy (\_ -> Json.Decode.map Delete (jsonDecObjId)))
            , ("Transform", Json.Decode.lazy (\_ -> Json.Decode.map3 Transform (Json.Decode.index 0 (Json.Decode.list (jsonDecObjId))) (Json.Decode.index 1 (Json.Decode.float)) (Json.Decode.index 2 (jsonDecVec2))))
            , ("Many", Json.Decode.lazy (\_ -> Json.Decode.map Many (Json.Decode.list (jsonDecEdit))))
            ]
    in  decodeSumObjectWithSingleField  "Edit" jsonDecDictEdit

jsonEncEdit : Edit -> Value
jsonEncEdit  val =
    let keyval v = case v of
                    Add v1 v2 -> ("Add", encodeValue (Json.Encode.list [jsonEncObjId v1, jsonEncObject v2]))
                    Delete v1 -> ("Delete", encodeValue (jsonEncObjId v1))
                    Transform v1 v2 v3 -> ("Transform", encodeValue (Json.Encode.list [(Json.Encode.list << List.map jsonEncObjId) v1, Json.Encode.float v2, jsonEncVec2 v3]))
                    Many v1 -> ("Many", encodeValue ((Json.Encode.list << List.map jsonEncEdit) v1))
    in encodeSumObjectWithSingleField keyval val



type Object  =
    ObjPoint {position: Vec2, radius: Float}
    | ObjBox Box

jsonDecObject : Json.Decode.Decoder ( Object )
jsonDecObject =
    let jsonDecDictObject = Dict.fromList
            [ ("ObjPoint", Json.Decode.lazy (\_ -> Json.Decode.map ObjPoint (   ("position" := jsonDecVec2) >>= \pposition ->    ("radius" := Json.Decode.float) >>= \pradius ->    Json.Decode.succeed {position = pposition, radius = pradius})))
            , ("ObjBox", Json.Decode.lazy (\_ -> Json.Decode.map ObjBox (jsonDecBox)))
            ]
    in  decodeSumObjectWithSingleField  "Object" jsonDecDictObject

jsonEncObject : Object -> Value
jsonEncObject  val =
    let keyval v = case v of
                    ObjPoint vs -> ("ObjPoint", encodeObject [("position", jsonEncVec2 vs.position), ("radius", Json.Encode.float vs.radius)])
                    ObjBox v1 -> ("ObjBox", encodeValue (jsonEncBox v1))
    in encodeSumObjectWithSingleField keyval val



type alias Document  =
   { undos: (List Edit)
   , redos: (List Edit)
   , instances: (Dict (Int, Int) Object)
   }

jsonDecDocument : Json.Decode.Decoder ( Document )
jsonDecDocument =
   ("undos" := Json.Decode.list (jsonDecEdit)) >>= \pundos ->
   ("redos" := Json.Decode.list (jsonDecEdit)) >>= \predos ->
   ("instances" := decodeMap (Json.Decode.map2 (,) (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.int))) (jsonDecObject)) >>= \pinstances ->
   Json.Decode.succeed {undos = pundos, redos = predos, instances = pinstances}

jsonEncDocument : Document -> Value
jsonEncDocument  val =
   Json.Encode.object
   [ ("undos", (Json.Encode.list << List.map jsonEncEdit) val.undos)
   , ("redos", (Json.Encode.list << List.map jsonEncEdit) val.redos)
   , ("instances", (encodeMap ((\(v1,v2) -> Json.Encode.list [(Json.Encode.int) v1,(Json.Encode.int) v2])) (jsonEncObject)) val.instances)
   ]



type ClientMsg  =
    ClientOpen DocName
    | ClientEdit DocName Edit

jsonDecClientMsg : Json.Decode.Decoder ( ClientMsg )
jsonDecClientMsg =
    let jsonDecDictClientMsg = Dict.fromList
            [ ("ClientOpen", Json.Decode.lazy (\_ -> Json.Decode.map ClientOpen (jsonDecDocName)))
            , ("ClientEdit", Json.Decode.lazy (\_ -> Json.Decode.map2 ClientEdit (Json.Decode.index 0 (jsonDecDocName)) (Json.Decode.index 1 (jsonDecEdit))))
            ]
    in  decodeSumObjectWithSingleField  "ClientMsg" jsonDecDictClientMsg

jsonEncClientMsg : ClientMsg -> Value
jsonEncClientMsg  val =
    let keyval v = case v of
                    ClientOpen v1 -> ("ClientOpen", encodeValue (jsonEncDocName v1))
                    ClientEdit v1 v2 -> ("ClientEdit", encodeValue (Json.Encode.list [jsonEncDocName v1, jsonEncEdit v2]))
    in encodeSumObjectWithSingleField keyval val



type ServerMsg  =
    ServerHello ClientId Dataset
    | ServerDocument Document
    | ServerOpen (Maybe DocName) Int DateTime
    | ServerEdit String Edit

jsonDecServerMsg : Json.Decode.Decoder ( ServerMsg )
jsonDecServerMsg =
    let jsonDecDictServerMsg = Dict.fromList
            [ ("ServerHello", Json.Decode.lazy (\_ -> Json.Decode.map2 ServerHello (Json.Decode.index 0 (jsonDecClientId)) (Json.Decode.index 1 (jsonDecDataset))))
            , ("ServerDocument", Json.Decode.lazy (\_ -> Json.Decode.map ServerDocument (jsonDecDocument)))
            , ("ServerOpen", Json.Decode.lazy (\_ -> Json.Decode.map3 ServerOpen (Json.Decode.index 0 (Json.Decode.maybe (jsonDecDocName))) (Json.Decode.index 1 (Json.Decode.int)) (Json.Decode.index 2 (jsonDecDateTime))))
            , ("ServerEdit", Json.Decode.lazy (\_ -> Json.Decode.map2 ServerEdit (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (jsonDecEdit))))
            ]
    in  decodeSumObjectWithSingleField  "ServerMsg" jsonDecDictServerMsg

jsonEncServerMsg : ServerMsg -> Value
jsonEncServerMsg  val =
    let keyval v = case v of
                    ServerHello v1 v2 -> ("ServerHello", encodeValue (Json.Encode.list [jsonEncClientId v1, jsonEncDataset v2]))
                    ServerDocument v1 -> ("ServerDocument", encodeValue (jsonEncDocument v1))
                    ServerOpen v1 v2 v3 -> ("ServerOpen", encodeValue (Json.Encode.list [(maybeEncode (jsonEncDocName)) v1, Json.Encode.int v2, jsonEncDateTime v3]))
                    ServerEdit v1 v2 -> ("ServerEdit", encodeValue (Json.Encode.list [Json.Encode.string v1, jsonEncEdit v2]))
    in encodeSumObjectWithSingleField keyval val



type alias DocInfo  =
   { modified: (Maybe DateTime)
   , included: Bool
   }

jsonDecDocInfo : Json.Decode.Decoder ( DocInfo )
jsonDecDocInfo =
   (Json.Decode.maybe ("modified" := jsonDecDateTime)) >>= \pmodified ->
   ("included" := Json.Decode.bool) >>= \pincluded ->
   Json.Decode.succeed {modified = pmodified, included = pincluded}

jsonEncDocInfo : DocInfo -> Value
jsonEncDocInfo  val =
   Json.Encode.object
   [ ("modified", (maybeEncode (jsonEncDateTime)) val.modified)
   , ("included", Json.Encode.bool val.included)
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
   { config: Config
   , images: (Dict String DocInfo)
   }

jsonDecDataset : Json.Decode.Decoder ( Dataset )
jsonDecDataset =
   ("config" := jsonDecConfig) >>= \pconfig ->
   ("images" := Json.Decode.dict (jsonDecDocInfo)) >>= \pimages ->
   Json.Decode.succeed {config = pconfig, images = pimages}

jsonEncDataset : Dataset -> Value
jsonEncDataset  val =
   Json.Encode.object
   [ ("config", jsonEncConfig val.config)
   , ("images", (encodeMap (Json.Encode.string) (jsonEncDocInfo)) val.images)
   ]



