module Scene.Document exposing (..)

-- import Vector as V exposing (..)
import Types exposing (Document, Edit(..), Object(..))

-- import Util exposing (..)
import Dict



type alias Index = Int


init : Document
init =
  { undos = []
  , redos = []
  , instances = Dict.empty
  }


getObject : Document -> Index -> Object
getObject doc i = case Dict.get i doc.instances of
  Just object -> object
  Nothing     -> Debug.crash ("getObject - missing index " ++ (toString i))



invertEdit : Document -> Edit -> Edit
invertEdit doc edit = case edit of
  Add i obj -> Delete i
  Delete  i -> Add i (getObject doc i)




add : Index -> Object -> Document -> Document
add i obj doc = { doc | instances = Dict.insert i obj doc.instances }

delete : Index -> Document -> Document
delete i doc = { doc | instances = Dict.remove i doc.instances }


modify : Index -> (Object -> Object) -> Document -> Document
modify i f doc = { doc | instances = Dict.update i (Maybe.map f) doc.instances }


maxIndex : Document -> Int
maxIndex doc = Maybe.withDefault -1 <| List.maximum
  (Dict.keys doc.instances ++ List.filterMap editIndex doc.undos ++ List.filterMap editIndex doc.redos)

editIndex : Edit -> Maybe Int
editIndex e = case e of
  Add i _ -> Just i
  Delete i -> Just i

applyEdit : Edit -> Document -> Document
applyEdit e doc = case e of
  Add    i obj -> add i obj doc
  Delete i     -> delete i doc
--
-- newEdit : Edit -> Document -> Document
-- newEdit e = let undo = invertEdit e
