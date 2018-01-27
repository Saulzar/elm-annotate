module Vector exposing (..)

import List
import Common exposing (Vec)
import Types exposing (Box, Extents)


type alias Size = Vec
type alias Position = Vec


vec : Float -> Float -> Vec
vec x y = Vec x y

centre : Box -> Position
centre box = scale 0.5 (add box.lower box.upper)

extents : Box -> Vec
extents box = scale 0.5 (sub box.upper box.lower)

boxSize : Box -> Vec
boxSize box = sub box.upper box.lower

toExtents : Box -> Extents
toExtents box = Extents (centre box) (extents box)


toBox : Extents -> Box
toBox ext = Box (sub ext.centre ext.extents) (add ext.centre ext.extents)


add : Vec -> Vec  -> Vec
add v1 v2 = Vec (v1.x + v2.x) (v1.y + v2.y)

sub : Vec -> Vec  -> Vec
sub v1 v2 = Vec (v1.x - v2.x) (v1.y - v2.y)


mul : Vec -> Vec  -> Vec
mul v1 v2 = Vec (v1.x * v2.x) (v1.y * v2.y)



neg : Vec  -> Vec
neg  = scale (-1)

scale :  Float -> Vec  -> Vec
scale s v = Vec (v.x * s) (v.y * s)

dot : Vec -> Vec  -> Float
dot v1 v2 = (v1.x * v2.x) + (v1.y * v2.y)


sum : List Vec -> Vec
sum vs = case vs of
  []        -> Vec  0 0
  (x :: xs) -> List.foldr add x xs

clamp : (Float, Float) -> Float -> Float
clamp (lower, upper) x = max (min x upper) lower
