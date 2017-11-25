module Scene.Document exposing (..)

import Vector as V exposing (..)
import Types exposing (Document, Edit(..), Object(..))

import Util exposing (..)
import List.Extra as List


type alias Index = Int



init : Document
init =
  { name = "noname"
  , undos = []
  , redos = []
  , instances = []
  }



getObject : Document -> Index -> Object
getObject doc i = case List.getAt i doc.instances of
  Just object -> object
  Nothing     -> Debug.crash ("getObject - missing index " ++ (toString i))



invertEdit : Document -> Edit -> Edit
invertEdit doc edit = case edit of
  Append i -> Delete (List.length doc.instances - 1)
  Insert i obj -> Delete i
  Delete i -> if List.length (doc.instances) <= i + 1
    then Insert i (getObject doc i)
    else Append (getObject doc i)



insert : Index -> Object -> Document -> Document
insert i obj doc = { doc | instances = insertAt i obj doc.instances }

delete : Index -> Document -> Document
delete i doc = { doc | instances = removeAt i doc.instances }


modify : Index -> (Object -> Object) -> Document -> Document
modify i f doc = { doc | instances = modifyAt i f doc.instances }



applyEdit : Edit -> Document -> Document
applyEdit e doc = case e of
  Append obj -> { doc | instances = obj :: doc.instances }
  Insert i obj -> insert i obj doc
  Delete i -> delete i doc
--
-- newEdit : Edit -> Document -> Document
-- newEdit e = let undo = invertEdit e
