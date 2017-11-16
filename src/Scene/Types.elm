module Scene.Types exposing (..)

import Input
import Vector as V exposing (Size, Position, Vector, Box)

import TypedSvg.Core exposing (Svg)
import Image exposing (Image)


type Command = Pan Vector | Zoom Float Position 




type alias Scene =
  {  background  : Maybe Image
  }


type alias Action =
  { update : (Input.Event, Input.State) -> Update
  , view   : () -> List (Svg Command)
  , cursor : String
  }


type Update = Continue Action (Maybe Command) | Ignored | End (Maybe Command)
