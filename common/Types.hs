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


data Edit
  = Add ObjId Object
  | Delete ObjId
  | Transform [ObjId] Float Vec
  | Many [Edit]

  deriving (Generic, Show, Eq)


data Object = ObjPoint {position :: Vec, radius :: Float} | ObjBox Box deriving (Generic, Show, Eq)

data Document = Document
  { undos :: [Edit]
  , redos :: [Edit]
  , instances :: Map ObjId Object
  } deriving (Generic, Show, Eq)


data DocInfo = DocInfo
  { modified :: Maybe DateTime
  , included :: Bool
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
  | ServerDocument DocName Document
  | ServerOpen (Maybe DocName) ClientId DateTime
  | ServerEdit DocName Edit
  | ServerEnd
      deriving (Generic, Show, Eq)

data ClientMsg
  = ClientOpen DocName
  | ClientEdit DocName Edit
  | ClientNext (Maybe DocName) 
      deriving (Generic, Show, Eq)


instance FromJSON Edit
instance FromJSON Object
instance FromJSON Document
instance FromJSON Config
instance FromJSON DocInfo
instance FromJSON Dataset
instance FromJSON ServerMsg
instance FromJSON ClientMsg


instance ToJSON Edit
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
