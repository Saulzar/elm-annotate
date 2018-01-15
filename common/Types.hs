module Types (
  module Types,
  module Geometry,

  module Data.Generics.Labels,
  Generic(..),
) where


import GHC.Generics

import qualified Data.Map as M
import Data.Map (Map)

import Data.Aeson (ToJSON(..), FromJSON(..))

import Data.Time.Clock

import Data.Generics.Product
import Data.Generics.Labels()

import Geometry


type ObjId = (Int, Int)

type ClientId = Int
type DocName = String

type DateTime = UTCTime

type Dim = (Int, Int)


data Edit
  = Add ObjId Object
  | Delete ObjId
  | Transform [ObjId] Float (V2 Float)
  | Many [Edit]

  deriving (Generic, Show, Eq)


data Object = ObjPoint {position :: (V2 Float), radius :: (V2 Float)} | ObjBox Box deriving (Generic, Show, Eq)

data Document = Document
  { undos :: [Edit]
  , redos :: [Edit]
  , instances :: Map (Int, Int) Object
  } deriving (Generic, Show, Eq)


data DocInfo = DocInfo
  { modified :: Maybe DateTime
  , included :: Bool
  , imageSize :: (Int, Int)
  } deriving (Generic, Show, Eq)


data Config = Config
  { extensions :: [String]
  } deriving (Generic, Show, Eq)

data Dataset = Dataset
  { config :: Config
  , images :: Map String DocInfo
  } deriving (Generic, Show, Eq)



data ServerMsg
  = ServerHello ClientId Dataset
  | ServerDocument Document
  | ServerOpen (Maybe DocName) Int DateTime
  | ServerEdit String Edit
      deriving (Generic, Show, Eq)

data ClientMsg
  = ClientOpen DocName
  | ClientEdit DocName Edit
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
