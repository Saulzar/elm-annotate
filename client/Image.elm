port module Image exposing (..)


import Vector exposing (Size)

type alias Image = { src : String, size : Size }

port loadImage : String -> Cmd msg
port imageLoaded  : (Image -> msg) -> Sub msg
