module Interaction exposing (..)


import Svg exposing (..)
import Svg.Attributes exposing (..)


import Input exposing (Event(..))
import Vector as V exposing (Size, Position, Vector, Box)


type Command = End

type alias Scene =
  {


  }

type Interaction = Interaction
  { update : Event -> Scene -> Interaction
  , view   : () -> Svg Command
  , cmd : Maybe Command
  }

interaction : (Event -> Scene -> Interaction) -> (() -> Svg Command) -> Maybe Command -> Interaction
interaction update view cmd = Interaction {update = update, view = view, cmd = cmd}

circle : Position -> Float -> Svg msg
circle pos radius = Svg.circle [cx (toString pos.x), cy (toString pos.y), r (toString radius)] []


type alias LineTool =
  { pos : Position
  }

initial : Position -> LineTool
initial p = { pos = p }

lineTool : LineTool -> Interaction
lineTool state = let
  update = \e _ -> case e of
      MouseMove p -> lineTool { state | pos = p }
      _ -> lineTool state


  view = always (circle state.pos 50)
    in interaction update view Nothing
