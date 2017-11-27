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



update : Action -> Update
update action = Continue (Just action) Nothing

command : Command -> Update
command cmd = Continue Nothing (Just cmd)


rec : ((a -> Action) -> a -> Action) -> a -> Action
rec f initial =
  let set = \state -> f set state
  in f set initial

end : Update
end = End Nothing


when : Bool -> Update -> Update
when b update = if b then update else Ignored

createObject : Scene -> Object -> Command
createObject scene = MakeEdit << (Add scene.nextId)

pan :  Position -> Action
pan = rec <| \set pos -> {action
  | update = \(e, _) scene -> case e of
      MouseMove mouse ->
        command (Pan pos mouse)

      MouseWheel deltas ->
        command (Zoom deltas.dy pos)

      MouseUp b   -> when (b == Mouse.Left) end

      _           -> Ignored

  , cursor = "move"
  }


circle : Position -> Float -> Svg msg
circle pos radius = Svg.circle [class ["brush"], cx (px pos.x), cy (px pos.y), r (px radius)] []



drawPoints : Key -> Position -> Action
drawPoints key = rec <| \set pos -> {action
    | update = \(e, _) scene ->
      case e of
          MouseMove mouse ->
            update (set (View.toLocal scene.view mouse))

          MouseWheel deltas ->
            command (ZoomBrush deltas.dy)

          Click b -> when (b == Mouse.Left) <|
            command (createObject scene (Point {position = pos, radius = scene.settings.brushRadius}))

          KeyUp k   -> when (k == key) end

          _           -> Ignored
    , view = \scene -> circle pos scene.settings.brushRadius

    , cursor = "none"
    }
