module Types where


import GHC.Generics

import Elm.Derive
import qualified Data.Map as M
import Data.Map (Map)


data Vec2 = Vec2 {x :: Float, y :: Float} deriving (Generic, Show)
data Edit
  = Add Int  Object
  | Delete Int
  | Transform [Int] Float Vec2
  | Many [Edit]

  deriving (Generic, Show)

data Box = Box { position :: Vec2, size :: Vec2 } deriving (Generic, Show)

data Object = ObjPoint {position :: Vec2, radius :: Float} | ObjBox Box deriving (Generic, Show)

data Document = Document
  { undos :: [Edit]
  , redos :: [Edit]

  , instances :: Map Int Object
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


data Response = RespDataset Dataset  | RespOpen String Document | RespError String | RespPong Int deriving (Generic, Show)
data Request = ReqDataset | ReqOpen String | ReqEdit Edit | ReqPing Int deriving (Generic, Show)



deriveBoth defaultOptions ''Vec2
deriveBoth defaultOptions ''Box

deriveBoth defaultOptions ''Edit
deriveBoth defaultOptions ''Object
deriveBoth defaultOptions ''Document

deriveBoth defaultOptions ''ImageInfo

deriveBoth defaultOptions ''Request
deriveBoth defaultOptions ''Response
deriveBoth defaultOptions ''Config

deriveBoth defaultOptions ''Dataset
