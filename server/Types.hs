module Types (
  module Types,
  module Data.Generics.Labels
) where


import GHC.Generics

import Elm.Derive
import qualified Data.Map as M
import Data.Map (Map)

import Data.Time.Clock

import Data.Generics.Product
import Data.Generics.Labels()

type ObjId = (Int, Int)

type ClientId = Int
type DocName = String

type DateTime = UTCTime


data Vec2 = Vec2 {x :: Float, y :: Float} deriving (Generic, Show)
data Edit
  = Add ObjId Object
  | Delete ObjId
  | Transform [ObjId] Float Vec2
  | Many [Edit]

  deriving (Generic, Show)

data Box = Box { position :: Vec2, size :: Vec2 } deriving (Generic, Show)
data Object = ObjPoint {position :: Vec2, radius :: Float} | ObjBox Box deriving (Generic, Show)

data Document = Document
  { undos :: [Edit]
  , redos :: [Edit]
  , instances :: Map ObjId Object
  } deriving (Generic, Show)


data DocInfo = DocInfo
  { modified :: Maybe DateTime
  , included :: Bool
  -- , imageSize :: (Int, Int)
  } deriving (Generic, Show)


data Config = Config
  { extensions :: [String]
  } deriving (Generic, Show)

data Dataset = Dataset
  { config :: Config
  , images :: Map DocName DocInfo
  } deriving (Generic, Show)



data ServerMsg
  = ServerHello ClientId Dataset
  | ServerDocument Document
  | ServerOpen (Maybe DocName) Int DateTime
  | ServerEdit String Edit
      deriving (Generic, Show)

data ClientMsg
  = ClientOpen DocName
  | ClientEdit DocName Edit
      deriving (Generic, Show)


deriveBoth defaultOptions ''Vec2
deriveBoth defaultOptions ''Box

deriveBoth defaultOptions ''Edit
deriveBoth defaultOptions ''Object
deriveBoth defaultOptions ''Document

deriveBoth defaultOptions ''DocInfo

deriveBoth defaultOptions ''ServerMsg
deriveBoth defaultOptions ''ClientMsg
deriveBoth defaultOptions ''Config
deriveBoth defaultOptions ''Dataset
