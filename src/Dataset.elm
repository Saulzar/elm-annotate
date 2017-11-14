module Dataset exposing (..)


type alias Dataset = {
  path : String,
  images : List Image
}

type alias Image = {
  file : String,
  annotated : Bool
}

dataset : Dataset
dataset = {
  path = "trees",
  images =
    [ {file = "_DSC2295.JPG", annotated = True}
    ,  {file = "small.JPG", annotated = False}
    ,  {file = "_DSC2300.JPG", annotated = True}
    ,  {file = "_DSC2301.JPG", annotated = False}
    ]
  }
