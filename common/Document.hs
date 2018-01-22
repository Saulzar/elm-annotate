module Document where

import Common

import qualified Data.Map as M
import Types

empty ::  Document
empty = Document
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
maximumId xs = Just $ foldl1 f xs where
  f (ObjId (o, c)) (ObjId (o', c')) = ObjId (max o o', max c c')


maxId :: Document -> Maybe ObjId
maxId Document{..} = maximumId $ catMaybes
  [ maxEdits undos
  , maxEdits redos
  , maximumId (M.keys instances)
  ]


applyEdit :: Edit -> Document -> Document
applyEdit = undefined

-- runEdit :: Edit -> Document -> Document
-- runEdit edit doc = doc' & #undos %~ (inverse:)
--   where (inverse, doc') = applyEdit edit doc
--
--
-- accumEdits :: Edit -> ([Edit], Document) -> Maybe ([Edit], Document)
-- accumEdits edit (inverses, doc) = (inv : inverses, doc') where
--     (inv, doc') = applyEdit edit doc
--
--
-- transformObj :: Float -> V2 -> Object -> Object
-- transformObj = undefined
--
-- instance Num V2 where
--   negate (V2 x y) = V2 (-x) (-y)
--
-- patchEdit :: Edit -> Map ObjId Object -> Maybe (Edit, Map ObjId Object)
-- patchEdit edit instances =  case edit of
--   Add k object -> return (Delete k, instances & M.insert k object)
--   Delete k     -> case (M.lookup k instances) of
--     Nothing     -> Nothing
--     Just object -> return (Add k object, instances &  M.delete k)
--   Transform ks s v -> return
--     ( Transform ks (1/s) (negate v)
--     , foldr (\k -> over (at k . traverse) (transformObj s v)) doc ks)
--
--   Many edits -> over _1 Many <$> foldr (flip accumEdits) ([], doc) edits
