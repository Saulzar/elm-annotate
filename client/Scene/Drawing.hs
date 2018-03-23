module Scene.Drawing where

import Common
import Miso (View, Attribute)

import Svg
import Geometry

viewCircle :: Position -> Float -> [Attribute action] -> View action
viewCircle (V2 x y) radius attributes = circle_ ([cx_ x, cy_ y, r_ radius] <> attributes) []


viewBox :: Box -> [Attribute action] -> View action
viewBox (Box l u) attributes = rect_ ([x_ x, y_ y, width_ w, height_ h] <> attributes) [] where
  (V2 w h) = u - l
  (V2 x y) = l
