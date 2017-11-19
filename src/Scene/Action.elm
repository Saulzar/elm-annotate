module Scene.Action exposing (..)


import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)
import TypedSvg as Svg exposing (..)
import TypedSvg.Attributes exposing (..)


import Input exposing (Event(..))

import Scene.View as View
import Vector as V exposing (Size, Position, Vector, Box)

import Scene.Types exposing (..)

import Input.Mouse as Mouse
import Keyboard.Key exposing (Key)




action : Action
action =
  { update = \_ _ -> end
  , cursor = "default"
  , view = always (g [] [])
  }




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

command : Action -> Command -> Update
command action act = Continue action (Just act)

end : Update
end = End Nothing



pan :  Position -> Action
pan pos = { action
  | update = \(e, _) scene -> case e of
      MouseMove mouse ->
        command (pan pos) (Pan pos mouse)

      MouseWheel deltas ->
        command (pan pos) (Zoom deltas.dy pos)

      MouseUp b   -> if b == Mouse.Left then end else Ignored

      _           -> Ignored

  , cursor = "move"
  }


circle : Position -> Float -> Svg msg
circle pos radius = Svg.circle [cx (px pos.x), cy (px pos.y), r (px radius)] []

drawPoints : Key -> Position -> Action
drawPoints key = let update = \pos -> { action
    | update = \(e, _) scene -> case e of
        MouseMove mouse ->
          continue (update (View.toLocal scene.view mouse))

        MouseWheel deltas ->
          command (update pos) (ZoomBrush deltas.dy)

        KeyUp k   -> if k == key then end else Ignored

        _           -> Ignored
    , view = \scene -> circle pos scene.settings.brushRadius

    , cursor = "none"
    } in update
