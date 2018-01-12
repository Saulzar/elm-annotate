module Types (
  module Types,
  module Data.Generics.Labels
) where


import GHC.Generics

import qualified Data.Map as M
import Data.Map (Map)

import Data.Aeson (ToJSON(..), FromJSON(..))

import Data.Time.Clock

import Data.Generics.Product
import Data.Generics.Labels()

type ObjId = (Int, Int)

type ClientId = Int
type DocName = String

type DateTime = UTCTime

type Dim = (Int, Int)


data Vec2 = Vec2 {x :: Float, y :: Float} deriving (Generic, Show, Eq)
data Edit
  = Add ObjId Object
  | Delete ObjId
  | Transform [ObjId] Float Vec2
  | Many [Edit]

  deriving (Generic, Show, Eq)

data Box = Box { position :: Vec2, size :: Vec2 } deriving (Generic, Show, Eq)
data Object = ObjPoint {position :: Vec2, radius :: Float} | ObjBox Box deriving (Generic, Show, Eq)

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

instance FromJSON Vec2
instance FromJSON Edit
instance FromJSON Box
instance FromJSON Object
instance FromJSON Document
instance FromJSON Config
instance FromJSON DocInfo
instance FromJSON Dataset
instance FromJSON ServerMsg
instance FromJSON ClientMsg


instance ToJSON Vec2
instance ToJSON Edit
instance ToJSON Box
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
