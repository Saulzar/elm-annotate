module Types where


import GHC.Generics

import Elm.Derive
import qualified Data.Map as M
import Data.Map (Map)


data Vec2 = Vec2 {x :: Float, y :: Float} deriving (Generic, Show)
data Edit = Append Object | Insert Int  Object | Delete Int deriving (Generic, Show)

data Object = Point {position :: Vec2, radius :: Float} | Box { min :: Vec2, max :: Vec2 } deriving (Generic, Show)

data Document = Document
  { name  :: String
  , undos :: [Edit]
  , redos :: [Edit]

  , instances :: Map Int Object
  , nextId :: Int
  } deriving (Generic, Show)


data ImageInfo = ImageInfo {
  file :: String,
  annotated :: Bool
} deriving (Generic, Show)


data Config = Config
  { extensions :: [String]
  } deriving (Generic, Show)


data Dataset = Dataset
  { path :: String
  , images :: [ImageInfo]
  , config :: Config
  } deriving (Generic, Show)


data Response = RespDataset Dataset  | RespEdit ImageInfo (Maybe Document) | RespError String | RespPong Int
data Request = ReqDataset | ReqEdit String | ReqPing Int



deriveBoth defaultOptions ''Vec2
deriveBoth defaultOptions ''Edit
deriveBoth defaultOptions ''Object
deriveBoth defaultOptions ''Document

deriveBoth defaultOptions ''ImageInfo

deriveBoth defaultOptions ''Request
deriveBoth defaultOptions ''Response
deriveBoth defaultOptions ''Config

deriveBoth defaultOptions ''Dataset
