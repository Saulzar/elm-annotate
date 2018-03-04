module Svg where

import Common

import Miso (Attribute)
import qualified Miso.Svg as S

import Miso.String (MisoString)
import qualified Miso.String as Str

import Miso.Html.Property (textProp)
import Data.JSString.RealFloat (formatFloat, FPFormat(..))

import Util

data Transform = Translate Float Float | Scale Float Float

transform_ ::  [Transform] -> Attribute action
transform_ ts = S.transform_ (Str.concat . intersperse " " $ transform' <$> ts)
  where
    transform' (Translate x y) = functionString "translate" [x, y]
    transform' (Scale x y)     = functionString "scale" [x, y]

viewBox_ ::   Float -> Float -> Float -> Float -> Attribute action
viewBox_ x y h w = arrayProp "viewBox" [x, y, w, h]

floatString :: Float -> MisoString
floatString = formatFloat Generic Nothing

functionString ::  MisoString -> [Float] -> MisoString
functionString name xs = name <> "(" <> floatArgs xs <> ")"

floatArgs ::   [Float] -> MisoString
floatArgs = Str.concat . intersperse " " . fmap floatString

arrayProp ::  MisoString -> [Float] -> Attribute action
arrayProp name xs = textProp name (floatArgs xs)

lengthProp ::  MisoString -> Float -> Attribute action
lengthProp name x = textProp name (floatString x)

x_ = lengthProp "x"
y_ = lengthProp "y"
width_ = lengthProp "width"
height_ = lengthProp "height"
cx_ = lengthProp "cx"
cy_ = lengthProp "cy"
r_ = lengthProp "r"


circle_ = S.circle_
rect_ = S.rect_

g_ = S.g_
svg_ = S.svg_
version_ = S.version_
image_ = S.image_

xlinkHref_ = textProp "href"
