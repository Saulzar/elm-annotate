module AppState where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.SafeCopy

import           Data.Typeable
import           System.FilePath

import qualified Data.Map as M
import Data.Map (Map)

import Types
import Control.Lens
import Control.Concurrent.STM

import GHC.Generics
import Data.Generics.Product.Subtype
import Data.Time.Clock

import Control.Concurrent.Log

import Codec.Picture

data AppState = AppState
  { config :: Config
  , images :: Map DocName DocInfo
  , documents :: Map DocName Document
  } deriving (Show, Generic)


data Command where
  CmdEdit :: DocName -> Edit -> Command
  CmdModified :: DocName -> UTCTime -> Command
  CmdImages :: [DocName] -> Command


$(deriveSafeCopy 0 'base ''Vec2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Object)
$(deriveSafeCopy 0 'base ''Edit)
$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 0 'base ''DocInfo)
$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''AppState)

$(deriveSafeCopy 0 'base ''Command)


docInfo :: DocName -> Traversal' AppState DocInfo
docInfo doc = #images . at doc . traverse


instance Persistable AppState where
  type Update AppState = Command

  update (CmdEdit doc edit) = undefined
  update (CmdModified doc time) =  docInfo doc . #modified .~ Just time


  update (CmdImages new) = over #images (M.union new')
    where
      new' = M.fromList $ (, emptyInfo) <$> new
      emptyInfo = DocInfo Nothing False


initialState :: Config -> AppState
initialState config = AppState
  { config = config
  , images = M.empty
  , documents = M.empty
  }


getImages :: Log AppState -> STM (Map DocName DocInfo)
getImages db = view #images <$> readCurrent db


updateImages :: [FilePath] -> Log AppState -> STM ()
updateImages images db = do
  existing <- getImages db 
  let new = filter (not . flip M.member existing) images

  updateLog db (CmdImages new)


getDataset :: AppState -> Dataset
getDataset = upcast

--

--
--
-- getConfig :: Query Storage Config
-- getConfig = asks $ view #config
--
--
-- getDocument :: FilePath -> Query Storage (Maybe (DocInfo, Maybe Document))
-- getDocument file = asks $ firstOf (#images . at file . traverse)
