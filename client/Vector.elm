module Vector exposing (..)

import List


type alias Vec2 = { x : Float, y : Float }

type alias Size = Vec2
type alias Position = Vec2

type alias Box =
  { position : Position
  , size     : Size
  }


v2 : Float -> Float -> Vec2
v2 x y = Vec2 x y

centre : Box -> Position
centre box = add box.position (scale 0.5 box.size)


add : Vec2 -> Vec2  -> Vec2
add v1 v2 = Vec2 (v1.x + v2.x) (v1.y + v2.y)

sub : Vec2 -> Vec2  -> Vec2
sub v1 v2 = Vec2 (v1.x - v2.x) (v1.y - v2.y)


mul : Vec2 -> Vec2  -> Vec2
mul v1 v2 = Vec2 (v1.x * v2.x) (v1.y * v2.y)



neg : Vec2  -> Vec2
neg  = scale (-1)

scale :  Float -> Vec2  -> Vec2
scale s v = Vec2 (v.x * s) (v.y * s)

dot : Vec2 -> Vec2  -> Float
dot v1 v2 = (v1.x * v2.x) + (v1.y * v2.y)


sum : List Vec2 -> Vec2
sum vs = case vs of
  []        -> Vec2  0 0
  (x :: xs) -> List.foldr add x xs

clamp : (Float, Float) -> Float -> Float
clamp (lower, upper) x = max (min x upper) lower
