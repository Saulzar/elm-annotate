module Scene.Interaction where

import Common
import Scene.Types
import Types

import Scene.Drawing

import qualified Web.KeyCode as Key
import Scene.Viewport (toLocal)
import Scene.Settings (zoomBy)

import Input  (Event(..), Button(..))
import Svg
import Miso (class_)


init :: Interaction
init = Interaction
  { update = \env e -> end
  , cursor = "default"
  , view = const (g_ [] [])
  , pending = []
  }

stateful :: ((a -> Command) -> a -> Interaction) -> a -> Interaction
stateful f  = f set where
  set state = Interact . Just $ f set state

whenCmd :: Monoid m => Bool -> m -> m
whenCmd True cmds = cmds
whenCmd _ _ = mempty

endCmd = Interact Nothing

end :: [Command]
end = [endCmd]

endWith :: [Command] -> [Command]
endWith = (endCmd :)

ignored :: [Command]
ignored = []

start :: Interaction -> Command
start = Interact . Just


createObject :: Env -> Object -> Command
createObject Env{..} obj = MakeEdit docName (Add nextId obj)


pan :: Position -> Interaction
pan = stateful $ \set pos -> init
  { update = \_ cmd -> case cmd of
      MouseMove mouse  -> [Pan pos mouse]
      MouseWheel delta -> [Zoom delta pos]
      MouseUp b   -> whenCmd (b == LeftButton) end

      _           -> ignored

  , cursor = "move"
  }



drawPoints :: Key.Key -> Position -> Interaction
drawPoints key = stateful $ \set pos -> init
  { update = \env@Env{..} -> \case
      MouseMove mouse   -> [set (toLocal viewport mouse)]
      MouseWheel delta  -> [ScaleBrush delta]

      Click b -> whenCmd (b == LeftButton) $
        [createObject env (ObjPoint pos (settings ^. #brushWidth))]

      KeyUp k   -> whenCmd (k == key) end
      _         -> ignored
    , view = \Env{..} -> circle pos (settings ^. #brushWidth) [class_ "brush"]
    , cursor = "none"
    }


dragSelected :: Button -> [ObjId] -> Position -> Interaction
dragSelected button selection origin = stateful dragObjects' (origin, 1)  where
  dragObjects' set (position, scale) = init
    { update = \Env {..} -> \case
          MouseMove mouse   -> [set (toLocal viewport mouse, scale)]
          MouseWheel delta  -> [set (position, scale * zoomBy delta)]
          MouseUp b         -> whenCmd (b == button) [MakeEdit docName edits, endCmd]
          _           -> ignored
    , cursor = "move"
    , pending = [edits]
    } where
        edits = Transform selection scale (position - origin)
