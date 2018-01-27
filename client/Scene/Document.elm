module Scene.Document exposing (..)

import Common exposing (..)
import Vector as V exposing (..)
import Types exposing (Document, Edit(..), Object(..), Box, Extents)

-- import Util exposing (..)
import Dict




init : Document
init =
  { undos = []
  , redos = []
  , instances = Dict.empty
  }

getObject : Document -> ObjId -> Object
getObject doc i = case Dict.get i doc.instances of
  Just object -> object
  Nothing     -> Debug.crash ("getObject - missing index " ++ (toString i))



add : ObjId -> Object -> Document -> Document
add i obj doc = { doc | instances = Dict.insert i obj doc.instances }

delete : ObjId -> Document -> Document
delete i doc = { doc | instances = Dict.remove i doc.instances }


modify :  (Object -> Object) -> ObjId -> Document -> Document
modify f i doc = { doc | instances = Dict.update i (Maybe.map f) doc.instances }


maxObject : Document -> Maybe Int
maxObject doc = List.maximum << List.filterMap List.maximum <|
  [  (Dict.keys doc.instances)
  ,  (List.filterMap maxEdit doc.undos)
  ,  (List.filterMap maxEdit doc.redos)
  ]

maxEdit : Edit -> Maybe Int
maxEdit e = case e of
  Add i _ -> Just i
  Delete i -> Just i
  Transform ids _ _ -> List.maximum ids
  Many edits -> List.maximum (List.filterMap maxEdit edits)


-- toExtents : { -> (Position, Vec)
-- toExtents b = let scale = V.scale 0.5 (V.sub b.min b.max)
--   in (V.add b.min scale, scale)

transformExtents : Float -> Vec -> Extents -> Extents
transformExtents s v b = { centre = V.add v b.centre, extents = V.scale s b.extents }

transform : Float -> Vec -> Object -> Object
transform s v obj = case obj of
  ObjPoint p -> ObjPoint {p | position = V.add p.position v, radius = p.radius * s}
  ObjBox b -> b |> toExtents |> transformExtents s v |> toBox |> ObjBox




accumEdits : Edit -> (List Edit, Document) -> (List Edit, Document)
accumEdits e (inverses, doc) = let (inv, doc_) = applyEdit_ e doc in (inv :: inverses, doc_)

applyEdit_ : Edit -> Document -> (Edit, Document)
applyEdit_ e doc = case e of
  Add    i obj -> (Delete i, add i obj doc)
  Delete i     -> (Add i (getObject doc i), delete i doc)
  Transform ids s v   -> (Transform ids (1/s) (V.neg v), List.foldr (modify (transform s v)) doc ids )
  Many edits ->
    let (inverses, doc_) = List.foldl accumEdits ([], doc) edits in (Many inverses, doc_)


applyEdit : Edit -> Document -> Document
applyEdit e doc =
  let (inv, doc_) = applyEdit_ e doc
  in {doc | undos = inv :: doc.undos}

--
-- newEdit : Edit -> Document -> Document
-- newEdit e = let undo = invertEdit e
