module Vector exposing (..)

type alias Vector = {x:Float, y:Float}

type alias Size = Vector
type alias Position = Vector

type alias Box =
  { position : Position
  , size     : Size
  }



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
