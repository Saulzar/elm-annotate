module Types (
  module Types,
  module Geometry,

  Generic(..),
) where

import Common

import qualified Data.Map as M

import Data.Generics.Product
import Geometry

newtype ObjId = ObjId { unObj :: (Int, ClientId) } deriving (Show, Ord, Eq, Generic, FromJSON, ToJSON, FromJSONKey, ToJSONKey)
newtype ClientId = ClientId { unClient :: Int } deriving (Show, Ord, Eq, Enum, Generic, FromJSON, ToJSON, FromJSONKey, ToJSONKey)
newtype DocName = DocName { unDoc :: String } deriving (Ord, Eq, Generic, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance Show DocName where
  show (DocName d) = d


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
  , instances :: Map ObjId Object
  } deriving (Generic, Show, Eq)


data DocInfo = DocInfo
  { modified :: Maybe UTCTime
  , included :: Bool
  , imageSize :: Dim
  } deriving (Generic, Show, Eq)


data Config = Config
  { extensions :: [String]
  } deriving (Generic, Show, Eq)

data Dataset = Dataset
  { config :: Config
  , images :: Map DocName DocInfo
  } deriving (Generic, Show, Eq)



data ServerMsg
  = ServerHello ClientId Dataset
  | ServerDocument DocName Document
  | ServerOpen (Maybe DocName) ClientId UTCTime
  | ServerEdit DocName Edit
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
