port module Image exposing (..)


import Vector exposing (Size)

type alias Image = { src : String, size : Size }

port loadImage : (String, String) -> Cmd msg
port imageLoaded  : ((String, Image) -> msg) -> Sub msg
