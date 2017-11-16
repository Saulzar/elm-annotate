module Scene.Action exposing (..)


import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)
import TypedSvg as Svg exposing (..)
import TypedSvg.Attributes exposing (..)


import Input exposing (Event(..))
import Vector as V exposing (Size, Position, Vector, Box)

import Input.Mouse as Mouse
import Scene.Types exposing (..)

action : Action
action =
  { update =  always end
  , cursor = "default"
  , view = always []
  }

circle : Position -> Float -> Svg msg
circle pos radius = Svg.circle [cx (px pos.x), cy (px pos.y), r (px radius)] []


-- type alias LineTool =
--   { pos : Position
--   }
--
-- initial : Position -> LineTool
-- initial p = { pos = p }
--
--
-- lineTool : LineTool -> Action
-- lineTool state = { action
--   | update = Update <| \e -> case e of
--       MouseMove p -> lineTool { state | pos = p })
--       _ -> Ignore
--
--   , view = always [circle state.pos 50]
-- }


continue : Action -> Update
continue action = Continue action Nothing

continueWith : Action -> Command -> Update
continueWith action act = Continue action (Just act)

end : Update
end = End Nothing


command : Command -> Action
command cmd = { action | update = always (End (Just cmd)) }

zoom :  Float -> Position -> Action
zoom zoom pos = command (Zoom zoom pos)


pan :  Position -> Action
pan pos = { action
  | update = \(e, _) -> case e of
      MouseMove mouse ->
        continueWith (pan mouse) (Pan (V.sub mouse pos))

      MouseWheel deltas ->
        continueWith (pan pos) (Zoom deltas.dy pos)

      MouseUp b   -> if b == Mouse.Left then end else Ignored

      Cancel      -> end
      _           -> Ignored

  , cursor = "move"
  }
