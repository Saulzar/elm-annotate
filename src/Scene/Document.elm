module Scene.Document exposing (..)

import Vector as V exposing (..)
import Dict exposing (Dict)


type alias Id = Int
type Edit = Insert Id Object | Delete Id

type Object = Point {position : Position, radius : Float}

type alias Document =
  { name  : String
  , undos : List Edit
  , redos : List Edit

  , instances : Dict Id Object
  , nextId : Id
  }



init : Document
init =
  { name = "noname"
  , undos = []
  , redos = []
  , instances = Dict.empty
  , nextId = 0
  }





getObject : Document -> Id -> Object
getObject doc id = case Dict.get id doc.instances of
  Just object -> object
  Nothing     -> Debug.crash ("getObject - missing id " ++ (toString id))


assertMissing : Id -> Document -> a -> a
assertMissing id doc = case Dict.get id doc.instances of
  Just object -> identity
  Nothing     -> Debug.crash ("assertMissing - id should be free " ++ (toString id))

invertEdit : Document -> Edit -> Edit
invertEdit doc edit = case edit of
  Insert id obj -> Delete id
  Delete id -> Insert id (getObject doc id)

insert : Id -> Object -> Document -> Document
insert id obj doc = { doc | instances = Dict.insert id obj doc.instances, nextId = max id doc.nextId + 1 }


delete : Id -> Document -> Document
delete id doc = { doc | instances = Dict.remove id doc.instances }


modify : Id -> (Object -> Object) -> Document -> Document
modify id f doc = { doc | instances = Dict.update id (Maybe.map f) doc.instances }



applyEdit : Edit -> Document -> Document
applyEdit e doc = case e of
  Insert id obj -> insert (doc.nextId) obj doc
  Delete id -> delete id doc
--
-- newEdit : Edit -> Document -> Document
-- newEdit e = let undo = invertEdit e
