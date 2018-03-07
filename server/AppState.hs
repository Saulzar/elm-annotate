module AppState where


import Common

import           Data.SafeCopy
import           System.FilePath

import qualified Data.Map as M

import Types
import Control.Lens
import Control.Concurrent.STM

import GHC.Generics
import Data.Generics.Product.Subtype
import Data.Time.Clock

import Linear.Affine

import Control.Concurrent.Log
import Data.SafeCopy

import Document (emptyDoc, applyEdit, applyCmd)

data AppState = AppState
  { config :: Config
  , images :: Map DocName DocInfo
  , documents :: Map DocName Document
  } deriving (Show, Eq, Generic)


data Command where
  CmdDoc :: DocName -> DocCmd -> UTCTime -> Command
  CmdCategory :: DocName -> ImageCat -> Command
  CmdModified :: DocName -> UTCTime -> Command
  CmdImages :: [(DocName, DocInfo)] -> Command
    deriving (Show, Eq, Generic)

$(deriveSafeCopy 0 'base ''V2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Extents)

$(deriveSafeCopy 0 'base ''Object)
$(deriveSafeCopy 0 'base ''Edit)
$(deriveSafeCopy 0 'base ''DocCmd)
$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 0 'base ''DocInfo)
$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''AppState)

$(deriveSafeCopy 0 'base ''Command)



docInfo :: DocName -> Traversal' AppState DocInfo
docInfo doc = #images . at doc . traverse


updateModified doc time = docInfo doc . #modified .~ Just time
updateDoc doc cmd = over (#documents . at doc) $ \maybeDoc ->
    Just $ applyCmd cmd (fromMaybe emptyDoc maybeDoc)

instance Persistable AppState where
  type Update AppState = Command

  update (CmdDoc doc cmd time) = updateModified doc time . updateDoc doc cmd
  update (CmdImages new) = over #images (M.union (M.fromList new))
  update (CmdCategory doc cat) = docInfo doc . #category .~ cat



initialState :: Config -> AppState
initialState config = AppState
  { config = config
  , images = M.empty
  , documents = M.empty
  }


lookupDoc :: DocName -> AppState -> (Maybe DocInfo, Maybe Document)
lookupDoc name AppState{..} = (M.lookup name images, M.lookup name documents)

getDataset :: AppState -> Dataset
getDataset = upcast



--
--
-- getConfig :: Query Storage Config
-- getConfig = asks $ view #config
--
--
-- getDocument :: FilePath -> Query Storage (Maybe (DocInfo, Maybe Document))
-- getDocument file = asks $ firstOf (#images . at file . traverse)
