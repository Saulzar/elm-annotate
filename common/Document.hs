module Document where

import Control.Lens
import Control.Monad

import GHC.Generics
import qualified Data.Map as M
import Data.Map (Map)

import Types

empty ::  Document
empty = Document
  { undos = []
  , redos = []
  , instances = M.empty
  }

-- runEdit :: Edit -> Document -> Document
-- runEdit edit doc = doc' & #undos %~ (inverse:)
--   where (inverse, doc') = applyEdit edit doc
--
--
-- accumEdits :: Edit -> ([Edit], Document) -> ([Edit], Document)
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
-- applyEdit :: Edit -> Document -> (Edit, Document)
-- applyEdit edit doc =  case edit of
--   Add k object -> (Delete k, doc & #instances %~ M.insert k object)
--   Delete k     -> case (M.lookup k (doc ^. #instances)) of
--     Nothing     -> (Null, doc)
--     Just object -> (Add k object, doc & #instances %~ M.delete k)
--   Transform ks s v ->
--     ( Transform ks (1/s) (negate v)
--     , foldr (\k -> over (#instances . at k . traverse) (transformObj s v)) doc ks)
--
--   Many edits -> over _1 Many <$> foldr (flip accumEdits) ([], doc) edits
