module Geometry
  ( module Linear.V2
  , module Linear.Vector
  , module Geometry
  ) where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..))

import Linear.V2
import Linear.Vector

data Box = Box { lower :: Vector, upper :: Vector } deriving (Generic, Show, Eq)
data Extents = Extents { centre :: Position, extents :: Vector } deriving (Generic, Show, Eq)


boxSize :: Box -> Vector
boxSize (Box l u) = u - l

centroid :: [Position] -> Position
centroid ps = sum ps / fromIntegral (length ps)

type Position = V2 Float
type Vector = V2 Float

type Size = V2 Float


instance FromJSON Box
instance FromJSON Extents
instance FromJSON a => FromJSON (V2 a)

instance ToJSON Box
instance ToJSON Extents
instance ToJSON a => ToJSON (V2 a)
