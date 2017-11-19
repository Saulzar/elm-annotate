module Scene.Document exposing (..)

import Vector as V exposing (..)
import Array exposing (Array)

type Id = Int
type Edit = Create Object | Delete Id


type Object = Point {position : Position, radius : Float}

type alias Document =
  { name  : String
  , undos : List Edit
  , redos : List Edit

  , instances : Array Object
  }



init : Document
init =
  { name = "noname"
  , undos = []
  , redos = []
  , instances = Array.empty
  }



edit : Edit -> Document -> Document
edit e doc = doc
