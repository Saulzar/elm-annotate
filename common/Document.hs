module Document where

import Common

import qualified Data.Map as M
import Types

emptyDoc ::  Document
emptyDoc = Document
  { undos = []
  , redos = []
  , instances = M.empty
  }


maxEdits :: [Edit] -> Maybe ObjId
maxEdits = maximumId . catMaybes . fmap maxEdit


maxEdit :: Edit -> Maybe ObjId
maxEdit (Add i _)  = Just i
maxEdit (Delete i) = Just i
maxEdit (Transform ids _ _) = maximumId ids
maxEdit (Many edits) = maxEdits edits

maximumId :: [ObjId] -> Maybe ObjId
maximumId [] = Nothing
maximumId xs = Just $ maximum xs


maxId :: Document -> Maybe ObjId
maxId Document{..} = maximumId $ catMaybes
  [ maxEdits undos
  , maxEdits redos
  , maximumId (M.keys instances)
  ]

type Content = Map ObjId Object

applyEdit :: Edit -> Document -> Document
applyEdit e doc = case patchEdit e (doc ^. #instances) of
  Nothing -> doc
  Just (inverse, content) -> doc
    & #instances .~ content
    & #undos %~ (inverse :)

accumEdits :: Edit -> ([Edit], Content) -> Maybe ([Edit], Content)
accumEdits edit (inverses, content) = do
    (inv, content') <- patchEdit edit content
    return (inv : inverses, content')


transformObj :: Float -> Vec -> Object -> Object
transformObj s t = \case
  ObjPoint p r -> ObjPoint (p + t) (r * s)
  ObjBox b     -> ObjBox $ b & boxExtents %~
    (\Extents{..} -> Extents (centre + t) (extents ^* s))

patchEdit :: Edit -> Content -> Maybe (Edit, Content)
patchEdit edit content =  case edit of
  Add k object -> return (Delete k, content & M.insert k object)
  Delete k     -> do
    object <- M.lookup k content
    return (Add k object, content &  M.delete k)

  Transform ks s v -> return
    ( Transform ks (1/s) (negate v)
    , foldr (\k -> over (at k . traverse) (transformObj s v)) content ks)

  Many edits ->  do
    (edits, content') <- foldM (flip accumEdits) ([], content) edits
    return (Many edits, content')
