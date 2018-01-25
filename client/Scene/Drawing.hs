module Scene.Drawing where

import Common
import Miso (View, Attribute)

import Svg
import Geometry

circle :: Position -> Float -> [Attribute action] -> View action
circle (V2 x y) radius attributes = circle_ ([cx_ x, cy_ y, r_ radius] <> attributes) []
