module Types (
  module Types,
  module Geometry,

  Generic(..),
) where

import Common

import qualified Data.Map as M

import Data.Generics.Product
import Geometry


type ObjId = Int
type ClientId = Int

type DocName = Text
type DateTime = UTCTime

data DocCmd = DocEdit Edit | DocUndo | DocRedo
  deriving (Show, Eq, Generic)

data Edit
  = Add [(ObjId, Object)]
  | Delete [ObjId]
  | Transform [ObjId] Float Vec
  | Many [Edit]
  -- | Undo
  -- | Redo

  deriving (Generic, Show, Eq)

instance Monoid Edit where
  mempty = Many []
  mappend (Many []) e = e
  mappend e (Many []) = e
  mappend e e' = Many [e, e']


data Object = ObjPoint {position :: Vec, radius :: Float} | ObjBox Box deriving (Generic, Show, Eq)

data Document = Document
  { undos :: [Edit]
  , redos :: [Edit]
  , instances :: Map ObjId Object
  } deriving (Generic, Show, Eq)


data ImageCat = New | Train | Test | Hold deriving (Eq, Ord, Enum, Generic, Show)

data DocInfo = DocInfo
  { modified :: Maybe DateTime
  , category :: ImageCat
  , imageSize :: (Int, Int)
  } deriving (Generic, Show, Eq)


data Config = Config
  { extensions :: [Text]
  } deriving (Generic, Show, Eq)

data Dataset = Dataset
  { config :: Config
  , images :: Map DocName DocInfo
  } deriving (Generic, Show, Eq)


data ServerMsg
  = ServerHello ClientId Dataset
  | ServerUpdateInfo DocName DocInfo
  | ServerDocument DocName DocInfo Document
  | ServerOpen (Maybe DocName) ClientId DateTime
  | ServerCmd DocName DocCmd
  | ServerEnd
      deriving (Generic, Show, Eq)

data ClientMsg
  = ClientOpen DocName
  | ClientCmd DocName DocCmd
  | ClientNext (Maybe DocName)
  | ClientSubmit DocName ImageCat
      deriving (Generic, Show, Eq)


instance FromJSON Edit
instance FromJSON DocCmd
instance FromJSON ImageCat
instance FromJSON Object
instance FromJSON Document
instance FromJSON Config
instance FromJSON DocInfo
instance FromJSON Dataset
instance FromJSON ServerMsg
instance FromJSON ClientMsg


instance ToJSON Edit
instance ToJSON DocCmd
instance ToJSON ImageCat
instance ToJSON Object
instance ToJSON Document
instance ToJSON Config
instance ToJSON DocInfo
instance ToJSON Dataset
instance ToJSON ServerMsg
instance ToJSON ClientMsg

defaultConfig :: Config
defaultConfig = Config
  { extensions = [".png", ".jpg", ".jpeg"]
  }
