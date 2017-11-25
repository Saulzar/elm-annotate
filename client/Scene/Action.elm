module Scene.Action exposing (..)


import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)
import TypedSvg as Svg exposing (..)
import TypedSvg.Attributes exposing (..)


import Input exposing (Event(..))

import Scene.View as View
-- import Scene.Document as Doc
import Types exposing (..)


import Vector as V exposing (Size, Position, Vec2, Box)

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


when : Bool -> Update -> Update
when b update = if b then update else Ignored

createObject : Scene -> Object -> Command
createObject scene = MakeEdit << Append

pan :  Position -> Action
pan pos = { action
  | update = \(e, _) scene -> case e of
      MouseMove mouse ->
        command (pan pos) (Pan pos mouse)

      MouseWheel deltas ->
        command (pan pos) (Zoom deltas.dy pos)

      MouseUp b   -> when (b == Mouse.Left) end

      _           -> Ignored

  , cursor = "move"
  }


circle : Position -> Float -> Svg msg
circle pos radius = Svg.circle [class ["brush"], cx (px pos.x), cy (px pos.y), r (px radius)] []

drawPoints : Key -> Position -> Action
drawPoints key = let update = \pos -> { action
    | update = \(e, _) scene ->
      let self = update pos
      in case e of
          MouseMove mouse ->
            continue (update (View.toLocal scene.view mouse))

          MouseWheel deltas ->
            command self (ZoomBrush deltas.dy)

          Click b -> when (b == Mouse.Left) <|
            command self (createObject scene (Point {position = pos, radius = scene.settings.brushRadius}))

          KeyUp k   -> when (k == key) end

          _           -> Ignored
    , view = \scene -> circle pos scene.settings.brushRadius

    , cursor = "none"
    } in update
