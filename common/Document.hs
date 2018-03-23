module Document where

import Common

import qualified Data.Map as M
import Types

import Data.List (uncons)

emptyDoc ::  Document
emptyDoc = Document
  { undos = []
  , redos = []
  , instances = M.empty
  }


maxEdits :: [Edit] -> Maybe ObjId
maxEdits = maximumId . catMaybes . fmap maxEdit


maxEdit :: Edit -> Maybe ObjId
maxEdit (Add objs)  = maximumId (fst <$> objs)
maxEdit (Delete ids) = maximumId ids
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
applyCmd cmd doc = fromMaybe doc $ snd <$> applyCmd' cmd doc

applyCmd' :: DocCmd -> Document -> Maybe ((Edit, Edit), Document)
applyCmd' DocUndo doc = applyUndo doc
applyCmd' DocRedo doc = applyRedo doc
applyCmd' (DocEdit e) doc = applyEdit e doc

applyEdit :: Edit -> Document -> Maybe ((Edit, Edit), Document)
applyEdit e doc = do
  (inverse, doc') <- applyEdit' e doc
  return ((e, inverse), doc'
    & #redos .~ mempty & #undos %~ (inverse :))

applyUndo :: Document -> Maybe ((Edit, Edit), Document)
applyUndo doc = do
  (e, undos) <- uncons (doc ^. #undos)
  (inverse, doc') <- applyEdit' e doc
  return ((e, inverse), doc'
    & #undos .~ undos & #redos %~ (inverse :))

applyRedo :: Document -> Maybe ((Edit, Edit), Document)
applyRedo doc = do
  (e, redos) <- uncons (doc ^. #redos)
  (inverse, doc') <- applyEdit' e doc
  return ((e, inverse), doc'
    & #redos .~ redos & #undos %~ (inverse :))


applyEdit' :: Edit -> Document -> Maybe (Edit, Document)
applyEdit' e doc = do
   (inverse, content) <- patchEdit e (doc ^. #instances)
   return (inverse, doc & #instances .~ content)


accumEdits :: Edit -> ([Edit], Content) -> Maybe ([Edit], Content)
accumEdits edit (inverses, content) = do
    (inv, content') <- patchEdit edit content
    return (inv : inverses, content')


transformObj :: Float -> Vec -> Object -> Object
transformObj s t = \case
  ObjPoint c p r -> ObjPoint c (p + t) (r * s)
  ObjBox c b     -> ObjBox c $ b & boxExtents %~
    (\Extents{..} -> Extents (centre + t) (extents ^* s))

patchEdit :: Edit -> Content -> Maybe (Edit, Content)
patchEdit edit content =  case edit of
  Add objs -> return (Delete (fst <$> objs), foldr (uncurry M.insert) content objs)
  Delete ks     -> do
    objs <- forM ks (\k -> (k,) <$> M.lookup k content)
    return (Add objs, foldr M.delete content ks)

  Transform ks s v -> return
    ( Transform ks (1/s) (negate v)
    , foldr (\k -> over (at k . traverse) (transformObj s v)) content ks)

  Many edits ->  do
    (edits, content') <- foldM (flip accumEdits) ([], content) edits
    return (Many edits, content')
