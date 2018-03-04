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


applyCmd :: DocCmd -> Document -> Document
applyCmd DocUndo doc = case doc ^. #undos of
  (x : xs) -> doc & applyUndo x & #undos .~ xs
  _        -> doc
applyCmd DocRedo doc = case doc ^. #redos of
  (x : xs) -> doc & applyRedo x & #redos .~ xs
  _        -> doc

applyCmd (DocEdit e) doc = applyEdit e doc

applyEdit :: Edit -> Document -> Document
applyEdit e = applyEdit' e (\inverse -> (#undos %~ (inverse :)) . (#redos .~ mempty))

applyUndo :: Edit -> Document -> Document
applyUndo e = applyEdit' e (\inverse -> #redos %~ (inverse :))

applyRedo :: Edit -> Document -> Document
applyRedo e = applyEdit' e (\inverse -> #undos %~ (inverse :))

applyEdit' :: Edit -> (Edit -> Document -> Document) -> Document -> Document
applyEdit' e f doc = case patchEdit e (doc ^. #instances) of
  Nothing -> doc
  Just (inverse, content) -> doc
    & #instances .~ content
    & f inverse

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
