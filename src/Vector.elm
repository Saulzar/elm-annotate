module Vector exposing (..)

import List

type alias Vector = {x:Float, y:Float}

type alias Size = Vector
type alias Position = Vector

type alias Box =
  { position : Position
  , size     : Size
  }


centre : Box -> Position
centre box = add box.position (scale 0.5 box.size)


add : Vector -> Vector -> Vector
add v1 v2 = Vector (v1.x + v2.x) (v1.y + v2.y)

sub : Vector -> Vector -> Vector
sub v1 v2 = Vector (v1.x - v2.x) (v1.y - v2.y)


mul : Vector -> Vector -> Vector
mul v1 v2 = Vector (v1.x * v2.x) (v1.y * v2.y)



neg : Vector  -> Vector
neg  = scale (-1)

scale :  Float -> Vector -> Vector
scale s v = Vector (v.x * s) (v.y * s)

dot : Vector -> Vector -> Float
dot v1 v2 = (v1.x * v2.x) + (v1.y * v2.y)


sum : List Vector -> Vector
sum vs = case vs of
  []        -> Vector 0 0
  (x :: xs) -> List.foldr add x xs

clamp : (Float, Float) -> Float -> Float
clamp (lower, upper) x = max (min x upper) lower
